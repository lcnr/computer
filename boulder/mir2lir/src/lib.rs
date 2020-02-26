#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use tindex::{TSlice, TVec};

use shared_id::{LocationId, TypeId};

use lir::Lir;
use mir::{Mir, Object, Type};

pub fn convert(mir: Mir) -> Lir {
    #[cfg(feature = "profiler")]
    profile_scope!("convert");
    let mut lir = Lir {
        functions: TVec::new(),
    };

    for function in mir.functions.into_iter() {
        lir.functions.push(convert_function(function, &mir.types));
    }

    lir
}

pub fn convert_function<'a>(
    mir: mir::Function<'a>,
    types: &TSlice<TypeId, Type>,
) -> lir::Function<'a> {
    #[cfg(feature = "profiler")]
    profile_scope!("convert_function");
    let mut blocks = TVec::new();

    for block in mir.blocks.into_iter() {
        blocks.push(convert_block(block, types));
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

fn convert_block(mir: mir::Block, types: &TSlice<TypeId, Type>) -> lir::Block {
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
    let mut step_offsets = TVec::new();
    for step in mir.steps.into_iter() {
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
            mir::Action::LoadConstant(obj) => {
                let data = convert_object(obj, step.ty, types);
                assert!(data.len() <= step_size);
                for (i, v) in data.into_iter().enumerate() {
                    if let Some(v) = v {
                        steps.push(lir::Action::LoadConstant(v, step_start + i));
                    }
                }
            }
            mir::Action::UnaryOperation(op, id) => match op {
                mir::UnaryOperation::Invert => {
                    assert_eq!(step_size, 1);
                    steps.push(lir::Action::Invert(step_offsets[id], step_start));
                }
                mir::UnaryOperation::Debug => {
                    for i in step_offsets[id].0..step_offsets[id + 1].0 {
                        steps.push(lir::Action::Debug(LocationId(i)));
                    }
                }
                mir::UnaryOperation::ToBytes | mir::UnaryOperation::FromBytes => unreachable!(),
            },
            mir::Action::InitializeUnion(_) | mir::Action::UnionFieldAccess(_) => unreachable!(),
            ref dk => unimplemented!("{:?}", dk),
        }
    }
    step_offsets.push(LocationId(memory_len));

    let terminator = match mir.terminator {
        mir::Terminator::Goto(target, args) => {
            let args = args
                .into_iter()
                .map(|arg| step_offsets[arg].0..step_offsets[arg + 1].0)
                .flatten()
                .map(LocationId)
                .collect();
            lir::Terminator::Goto(target, args)
        }
        mir::Terminator::MatchByte(on, arms) => {
            let arms = arms
                .into_iter()
                .map(|arm| lir::MatchArm {
                    pat: arm.pat,
                    target: arm.target,
                    args: arm
                        .args
                        .into_iter()
                        .map(|arg| arg.unwrap_or(on))
                        .map(|arg| step_offsets[arg].0..step_offsets[arg + 1].0)
                        .flatten()
                        .map(LocationId)
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

fn convert_object(obj: Object, ty: TypeId, types: &TSlice<TypeId, Type>) -> Vec<Option<u8>> {
    match obj {
        Object::U8(v) => vec![Some(v)],
        Object::Field(id, obj) => convert_object(*obj, id, types),
        Object::Undefined | Object::Unit => vec![],
        Object::Struct(fields) => fields
            .into_iter()
            .zip(types[ty].expect_struct().iter())
            .map(|(field, &ty)| {
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
