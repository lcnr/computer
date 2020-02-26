#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use tindex::{tvec, TSlice, TVec};

use shared_id::{FieldId, LocationId, StepId, TypeId};

use lir::Lir;
use mir::{Mir, Object, Type};

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

    for block in mir.blocks.into_iter() {
        blocks.push(convert_block(block, field_offsets, types));
    }

    let ctx = lir::FunctionContext {
        test: mir.ctx.is_test,
        export: mir.ctx.export,
        hidden: mir.ctx.hidden,
    };

    lir::Function {
        name: mir.name,
        ctx,
        blocks,
        return_length: types[mir.ret].size(types),
    }
}

fn convert_block(
    mir: mir::Block,
    field_offsets: &TSlice<TypeId, TVec<FieldId, usize>>,
    types: &TSlice<TypeId, Type>,
) -> lir::Block {
    #[cfg(feature = "profiler")]
    profile_scope!("convert_block");
    let mut input_offsets = TVec::new();
    input_offsets.push(0);
    for &input in mir.input.iter() {
        let &prev = input_offsets.last().unwrap();
        input_offsets.push(prev + types[input].size(types));
    }
    let mut memory_len = 0;

    let mut steps = TVec::new();
    let mut step_offsets: TVec<StepId, LocationId> = TVec::new();
    for step in mir.steps.iter() {
        let step_size = types[step.ty].size(types);
        let step_start = LocationId(memory_len);
        step_offsets.push(step_start);
        memory_len += step_size;

        match step.action {
            mir::Action::Extend(_) | mir::Action::Reduce(_) => unreachable!(),
            mir::Action::LoadInput(id) => {
                let input_start = input_offsets[id];
                for i in 0..step_size {
                    steps.push(lir::Action::LoadInput(input_start + i, step_start + i));
                }
            }
            mir::Action::LoadConstant(ref obj) => {
                let data = convert_object(obj, step.ty, types);
                assert!(data.len() <= step_size);
                for (i, v) in data.into_iter().enumerate() {
                    if let Some(v) = v {
                        steps.push(lir::Action::LoadConstant(v, step_start + i));
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
                            step_offsets[fields[f]] + i,
                            step_start + field_offset + i,
                        ));
                    }
                }
            }
            mir::Action::CallFunction(id, ref args) => {
                let so = &step_offsets;
                let args = args
                    .iter()
                    .map(|&arg| (0..types[mir.steps[arg].ty].size(types)).map(move |v| so[arg] + v))
                    .flatten()
                    .collect();

                let ret = (step_start.0..memory_len).map(LocationId).collect();
                steps.push(lir::Action::FunctionCall { id, args, ret });
            }
            mir::Action::StructFieldAccess(step, field) => {
                let step_offset = step_offsets[step] + field_offsets[mir.steps[step].ty][field];
                *step_offsets.last_mut().unwrap() = step_offset;
                memory_len -= step_size;
            }
            mir::Action::UnaryOperation(op, id) => match op {
                mir::UnaryOperation::Invert => {
                    assert_eq!(step_size, 1);
                    steps.push(lir::Action::Invert(step_offsets[id], step_start));
                }
                mir::UnaryOperation::Debug => {
                    let ty = mir.steps[id].ty;
                    for i in 0..types[ty].size(types) {
                        steps.push(lir::Action::Debug(step_offsets[id] + i));
                    }
                }
                mir::UnaryOperation::ToBytes | mir::UnaryOperation::FromBytes => unreachable!(),
            },
            mir::Action::Binop(op, l, r) => {
                assert_eq!(step_size, 1);
                steps.push(lir::Action::Binop {
                    op: convert_binop(op),
                    l: step_offsets[l],
                    r: step_offsets[r],
                    out: step_start,
                });
            }
            mir::Action::InitializeUnion(id) => {
                for i in 0..types[mir.steps[id].ty].size(types) {
                    steps.push(lir::Action::Move(step_offsets[id] + i, step_start + i));
                }
            }
            mir::Action::UnionFieldAccess(id) => {
                for i in 0..step_size {
                    steps.push(lir::Action::Move(step_offsets[id] + i, step_start + i));
                }
            }
        }
    }

    let terminator = match mir.terminator {
        mir::Terminator::Goto(target, ref args) => {
            let so = &step_offsets;
            let args = args
                .iter()
                .map(|&arg| (0..types[mir.steps[arg].ty].size(types)).map(move |v| so[arg] + v))
                .flatten()
                .collect();
            lir::Terminator::Goto(target, args)
        }
        mir::Terminator::MatchByte(on, ref arms) => {
            let so = &step_offsets;
            let arms = arms
                .iter()
                .map(|arm| lir::MatchArm {
                    pat: arm.pat,
                    target: arm.target,
                    args: arm
                        .args
                        .iter()
                        .map(|&arg| arg.unwrap_or(on))
                        .map(|arg| {
                            (0..types[mir.steps[arg].ty].size(types)).map(move |v| so[arg] + v)
                        })
                        .flatten()
                        .collect(),
                })
                .collect();
            lir::Terminator::Match(step_offsets[on], arms)
        }
        mir::Terminator::Match(_, _) => unreachable!(),
    };

    lir::Block {
        input_len: input_offsets.pop().unwrap(),
        memory_len,
        steps,
        terminator,
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
