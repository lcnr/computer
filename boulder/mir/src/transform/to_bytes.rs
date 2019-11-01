use std::iter;

use tindex::TVec;

use shared_id::{U16_TYPE_ID, U32_TYPE_ID, U8_TYPE_ID};

use crate::{
    Action, Binop, Block, BlockId, FieldId, Function, Mir, Object, Step, StepId, Type,
    UnaryOperation,
};

impl<'a> Mir<'a> {
    /// reduce all `u32` and `u16` to `u8`
    pub fn reduce_to_bytes(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::reduce_sum_types");
        self.types[U32_TYPE_ID] =
            Type::Struct(tvec![U8_TYPE_ID, U8_TYPE_ID, U8_TYPE_ID, U8_TYPE_ID]);
        self.types[U16_TYPE_ID] = Type::Struct(tvec![U8_TYPE_ID, U8_TYPE_ID]);
        for func in self.functions.iter_mut() {
            func.reduce_to_bytes();
        }
    }
}

fn invert_u16() -> TVec<StepId, Step> {
    let mut new_steps = TVec::new();
    let high = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(0)),
    ));
    let high_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, high),
    ));
    let low = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(1)),
    ));
    let low_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, low),
    ));
    new_steps.push(Step::new(
        U16_TYPE_ID,
        Action::InitializeStruct(tvec![high_inv, low_inv]),
    ));

    new_steps
}

fn invert_u32() -> TVec<StepId, Step> {
    let mut new_steps = TVec::new();
    let high = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(0)),
    ));
    let high_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, high),
    ));
    let high_middle = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(1)),
    ));
    let high_middle_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, high_middle),
    ));
    let low_middle = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(2)),
    ));
    let low_middle_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, low_middle),
    ));
    let low = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(3)),
    ));
    let low_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, low),
    ));
    new_steps.push(Step::new(
        U32_TYPE_ID,
        Action::InitializeStruct(tvec![high_inv, high_middle_inv, low_middle_inv, low_inv]),
    ));
    new_steps
}

impl<'a> Function<'a> {
    fn reduce_to_bytes(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Function::reduce_sum_types");
        let mut block_id = BlockId::from(0);
        while block_id.0 < self.blocks.len() {
            let current_block = &mut self.blocks[block_id];
            let mut step_id = StepId::from(0);
            while step_id.0 < current_block.steps.len() {
                match &mut current_block.steps[step_id].action {
                    &mut Action::LoadConstant(ref mut obj) => {
                        obj.reduce_to_bytes();
                    }
                    &mut Action::UnaryOperation(op, target_id) => {
                        let new_steps = match (current_block.steps[target_id].ty, op) {
                            (U16_TYPE_ID, UnaryOperation::Invert) => invert_u16(),
                            (U32_TYPE_ID, UnaryOperation::Invert) => invert_u32(),
                            (_, UnaryOperation::Invert) => {
                                step_id.0 += 1;
                                continue;
                            }
                        };
                        step_id = current_block.insert_steps(
                            step_id..=step_id,
                            new_steps,
                            iter::once(target_id),
                        );
                    }
                    &mut Action::Binop(op, a, b) => {
                        let new_steps =
                            match (current_block.steps[a].ty, current_block.steps[b].ty, op) {
                                (U16_TYPE_ID, U16_TYPE_ID, Binop::Add) => unimplemented!("u16 add"),
                                (U32_TYPE_ID, U32_TYPE_ID, Binop::Add) => unimplemented!("u32 add"),
                                (_, _, Binop::Add) => (),
                                (U16_TYPE_ID, U16_TYPE_ID, Binop::Sub) => unimplemented!("u16 sub"),
                                (U32_TYPE_ID, U32_TYPE_ID, Binop::Sub) => unimplemented!("u32 sub"),
                                (_, _, Binop::Sub) => (),
                                (_, _, Binop::Mul) | (_, _, Binop::Div) | (_, _, Binop::Rem) => {
                                    unreachable!("to_bytes called with extended binops: {:?}", op)
                                }
                                (U16_TYPE_ID, U16_TYPE_ID, Binop::Shl) => unimplemented!("u16 shl"),
                                (U32_TYPE_ID, U32_TYPE_ID, Binop::Shl) => unimplemented!("u32 shl"),
                                (_, _, Binop::Shl) => (),
                                (U16_TYPE_ID, U16_TYPE_ID, Binop::Shr) => unimplemented!("u16 shr"),
                                (U32_TYPE_ID, U32_TYPE_ID, Binop::Shr) => unimplemented!("u32 shr"),
                                (_, _, Binop::Shr) => (),
                                (U16_TYPE_ID, U16_TYPE_ID, Binop::Eq) => unimplemented!("u16 eq"),
                                (U32_TYPE_ID, U32_TYPE_ID, Binop::Eq) => unimplemented!("u32 eq"),
                                (_, _, Binop::Eq) => (),
                                (U16_TYPE_ID, U16_TYPE_ID, Binop::Neq) => unimplemented!("u16 neq"),
                                (U32_TYPE_ID, U32_TYPE_ID, Binop::Neq) => unimplemented!("u32 neq"),
                                (_, _, Binop::Neq) => (),
                                (U16_TYPE_ID, U16_TYPE_ID, Binop::Gt) => unimplemented!("u16 gt"),
                                (U32_TYPE_ID, U32_TYPE_ID, Binop::Gt) => unimplemented!("u32 gt"),
                                (_, _, Binop::Gt) => (),
                                (U16_TYPE_ID, U16_TYPE_ID, Binop::Gte) => unimplemented!("u16 gte"),
                                (U32_TYPE_ID, U32_TYPE_ID, Binop::Gte) => unimplemented!("u32 gte"),
                                (_, _, Binop::Gte) => (),
                                (U16_TYPE_ID, U16_TYPE_ID, Binop::BitOr) => {
                                    unimplemented!("u16 or")
                                }
                                (U32_TYPE_ID, U32_TYPE_ID, Binop::BitOr) => {
                                    unimplemented!("u32 or")
                                }
                                (_, _, Binop::BitOr) => (),
                                (U16_TYPE_ID, U16_TYPE_ID, Binop::BitAnd) => {
                                    unimplemented!("u16 and")
                                }
                                (U32_TYPE_ID, U32_TYPE_ID, Binop::BitAnd) => {
                                    unimplemented!("u32 and")
                                }
                                (_, _, Binop::BitAnd) => (),
                            };
                        step_id.0 += 1;
                    }
                    _ => step_id.0 += 1,
                }
            }
            block_id.0 += 1;
        }
    }
}

impl Object {
    fn reduce_to_bytes(&mut self) {
        match self {
            &mut Object::Unit | &mut Object::U8(_) | &mut Object::Undefined => (),
            &mut Object::U16(val) => {
                *self = Object::Struct(val.to_be_bytes().iter().copied().map(Object::U8).collect())
            }
            &mut Object::U32(val) => {
                *self = Object::Struct(val.to_be_bytes().iter().copied().map(Object::U8).collect())
            }
            &mut Object::Struct(ref mut content) => {
                content.iter_mut().for_each(Self::reduce_to_bytes)
            }
            &mut Object::Variant(_, ref mut obj) => obj.reduce_to_bytes(),
            &mut Object::Field(_, ref mut obj) => obj.reduce_to_bytes(),
        }
    }
}
