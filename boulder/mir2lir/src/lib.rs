#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use tindex::{tvec, TSlice, TVec};

use shared_id::{BlockId, FieldId, LocationId, StepId, TypeId};

use lir::{traits::Update, Arg, Lir};
use mir::{Mir, Object, Type};

use std::cmp;

pub fn convert(mir: Mir) -> Lir {
    #[cfg(feature = "profiler")]
    profile_scope!("convert");
    let mut lir = Lir {
        ctx: lir::Context {
            true_replacement: mir.ctx.true_replacement,
            false_replacement: mir.ctx.false_replacement,
        },
        functions: TVec::new(),
    };

    let field_offsets: TVec<TypeId, TVec<FieldId, usize>> = mir
        .types
        .iter()
        .map(|t| {
            if let Type::Struct(fields) = t {
                let mut offsets = tvec![0];
                for &field in fields {
                    let size = mir.types[field].size(&mir.types);
                    let offset = offsets.last().unwrap() + size;
                    offsets.push(offset);
                }
                offsets.pop();
                offsets
            } else {
                TVec::new()
            }
        })
        .collect();

    for function in mir.functions.into_iter() {
        lir.functions
            .push(convert_function(function, &field_offsets, &mir.types));
    }

    lir
}

pub fn convert_function<'a>(
    mir: mir::Function<'a>,
    field_offsets: &TSlice<TypeId, TVec<FieldId, usize>>,
    types: &TSlice<TypeId, Type>,
) -> lir::Function<'a> {
    #[cfg(feature = "profiler")]
    profile_scope!("convert_function");
    let mut blocks = TVec::new();
    let mut replacement_blocks = Vec::new();
    let mut replacements = BlockId::replacement_iter();

    let mut memory_len = 0;
    for block in mir.blocks.iter() {
        let mut input_offsets = TVec::new();
        input_offsets.push(LocationId(0));
        for &input in block.input.iter() {
            let &prev = input_offsets.last().unwrap();
            input_offsets.push(prev + types[input].size(types));
        }

        let mut block_mem = input_offsets.pop().unwrap().0;

        let mut steps = TVec::new();
        let mut step_offsets: TVec<StepId, LocationId> = TVec::new();
        for step in block.steps.iter() {
            let step_size = types[step.ty].size(types);
            let step_start = LocationId(block_mem);
            step_offsets.push(step_start);
            block_mem += step_size;

            match step.action {
                mir::Action::Extend(_) | mir::Action::Reduce(_) => unreachable!(),
                mir::Action::LoadInput(id) => {
                    *step_offsets.last_mut().unwrap() = input_offsets[id];
                    block_mem -= step_size;
                }
                mir::Action::LoadConstant(ref obj) => {
                    let data = convert_object(obj, step.ty, types);
                    assert!(data.len() <= step_size);
                    for (i, v) in data.into_iter().enumerate() {
                        if let Some(v) = v {
                            steps.push(lir::Action::Move(lir::Arg::Byte(v), step_start + i));
                        }
                    }
                }
                mir::Action::InitializeStruct(ref fields) => {
                    let field_types = types[step.ty].expect_struct();
                    let offsets = &field_offsets[step.ty];
                    for f in fields.index_iter() {
                        let field_offset = offsets[f];
                        for i in 0..types[field_types[f]].size(types) {
                            steps.push(lir::Action::Move(
                                lir::Arg::Location(step_offsets[fields[f]] + i),
                                step_start + field_offset + i,
                            ));
                        }
                    }
                }
                mir::Action::CallFunction(id, ref args) => {
                    let so = &step_offsets;
                    let args = args
                        .iter()
                        .map(|&arg| {
                            (0..types[block.steps[arg].ty].size(types)).map(move |v| so[arg] + v)
                        })
                        .flatten()
                        .map(Arg::Location)
                        .map(Some)
                        .collect();

                    let ret = (step_start.0..block_mem)
                        .map(LocationId)
                        .map(Some)
                        .collect();
                    steps.push(lir::Action::FunctionCall { id, args, ret });
                }
                mir::Action::StructFieldAccess(step, field) => {
                    let step_offset =
                        step_offsets[step] + field_offsets[block.steps[step].ty][field];
                    *step_offsets.last_mut().unwrap() = step_offset;
                    block_mem -= step_size;
                }
                mir::Action::UnaryOperation(op, id) => match op {
                    mir::UnaryOperation::Invert => {
                        assert_eq!(step_size, 1);
                        steps.push(lir::Action::Invert(step_offsets[id], step_start));
                    }
                    mir::UnaryOperation::Debug => {
                        let ty = block.steps[id].ty;
                        for i in 0..types[ty].size(types) {
                            steps.push(lir::Action::Debug(step_offsets[id] + i));
                        }
                    }
                    mir::UnaryOperation::BlackBox => {
                        let ty = block.steps[id].ty;
                        for i in 0..types[ty].size(types) {
                            steps.push(lir::Action::BlackBox(step_offsets[id] + i, step_start + i));
                        }
                    }
                    mir::UnaryOperation::ToBytes | mir::UnaryOperation::FromBytes => unreachable!(),
                },
                mir::Action::Binop(op, l, r) => {
                    assert_eq!(step_size, 1);
                    steps.push(lir::Action::Binop {
                        op: convert_binop(op),
                        l: Arg::Location(step_offsets[l]),
                        r: Arg::Location(step_offsets[r]),
                        out: step_start,
                    });
                }
                mir::Action::InitializeUnion(step) | mir::Action::UnionFieldAccess(step) => {
                    let step_offset = step_offsets[step];
                    *step_offsets.last_mut().unwrap() = step_offset;
                    block_mem -= step_size;
                }
            }
        }

        let terminator = match block.terminator {
            mir::Terminator::Goto(target, ref args) => {
                let so = &step_offsets;
                steps.extend(
                    args.iter()
                        .map(|&arg| {
                            (0..types[block.steps[arg].ty].size(types)).map(move |v| so[arg] + v)
                        })
                        .flatten()
                        .enumerate()
                        .map(|(i, loc)| lir::Action::Move(lir::Arg::Location(loc), LocationId(i))),
                );
                lir::Terminator::Goto(target)
            }
            mir::Terminator::MatchByte(on, ref arms) => {
                let so = &step_offsets;
                for arm in arms.iter() {
                    replacement_blocks.push(lir::Block {
                        steps: arm
                            .args
                            .iter()
                            .map(|&arg| arg.unwrap_or(on))
                            .flat_map(|arg| {
                                (0..types[block.steps[arg].ty].size(types))
                                    .map(move |v| so[arg] + v)
                            })
                            .enumerate()
                            .map(|(pos, arg)| {
                                lir::Action::Move(lir::Arg::Location(arg), LocationId(pos))
                            })
                            .collect(),
                        terminator: lir::Terminator::Goto(arm.target),
                    });
                }

                lir::Terminator::Match(
                    step_offsets[on],
                    arms.iter()
                        .map(|arm| lir::MatchArm {
                            pat: arm.pat,
                            target: Some(replacements.next().unwrap()),
                        })
                        .collect(),
                )
            }
            mir::Terminator::Match(_, _) => unreachable!(),
        };

        memory_len = cmp::max(memory_len, block_mem);
        blocks.push(lir::Block { steps, terminator });
    }

    let replacement_start = blocks.range_end();
    let last_replacement = replacements.next().unwrap();
    blocks.extend(replacement_blocks);
    for block in blocks.iter_mut() {
        block.update(|block_id: BlockId| {
            if block_id > last_replacement {
                replacement_start + (usize::MAX - block_id.0)
            } else {
                block_id
            }
        })
    }

    let ctx = lir::FunctionContext {
        inline: mir.ctx.inline,
        test: mir.ctx.is_test,
        export: mir.ctx.export,
        hidden: mir.ctx.hidden,
    };

    lir::Function {
        name: mir.name,
        ctx,
        blocks,
        input_len: mir.blocks[BlockId(0)]
            .input
            .iter()
            .map(|&input| types[input].size(types))
            .sum(),
        memory_len,
        return_len: types[mir.ret].size(types),
    }
}

fn convert_object(obj: &Object, ty: TypeId, types: &TSlice<TypeId, Type>) -> Vec<Option<u8>> {
    match *obj {
        Object::U8(v) => vec![Some(v)],
        Object::Field(id, ref obj) => convert_object(obj, id, types),
        Object::Undefined | Object::Unit => vec![],
        Object::Struct(ref fields) => fields
            .iter()
            .zip(types[ty].expect_struct().iter())
            .map(|(field, &ty)| {
                // FIXME: consider using `field_offsets` instead
                let field_size = types[ty].size(types);
                let mut field_data = convert_object(field, ty, types);
                assert!(field_data.len() <= field_size);
                field_data.resize(field_size, None);
                field_data
            })
            .flatten()
            .collect(),
        Object::U16(_) | Object::U32(_) | Object::Variant(_, _) => unreachable!(),
    }
}

fn convert_binop(op: mir::binop::Binop) -> lir::Binop {
    use lir::Binop as L;
    use mir::binop::Binop as M;

    match op {
        M::Add => L::Add,
        M::Sub => L::Sub,
        M::Shl => L::Shl,
        M::Shr => L::Shr,
        M::Eq => L::Eq,
        M::Neq => L::Neq,
        M::Gt => L::Gt,
        M::Gte => L::Gte,
        M::BitOr => L::BitOr,
        M::BitAnd => L::BitAnd,
        M::BitXor => L::BitXor,
        M::Mul | M::Div | M::Rem => unreachable!(),
    }
}
