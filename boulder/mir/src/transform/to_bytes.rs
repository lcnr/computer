use std::iter;

use tindex::TVec;

use shared_id::{U16_TYPE_ID, U32_TYPE_ID, U8_TYPE_ID};

use crate::{Action, Block, FieldId, Mir, Object, Step, StepId, Type, UnaryOperation, Binop};

impl<'a> Mir<'a> {
    /// reduce all `u32` and `u16` to `u8`
    pub fn reduce_to_bytes(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::reduce_sum_types");
        self.types[U32_TYPE_ID] =
            Type::Struct(tvec![U8_TYPE_ID, U8_TYPE_ID, U8_TYPE_ID, U8_TYPE_ID]);
        self.types[U16_TYPE_ID] = Type::Struct(tvec![U8_TYPE_ID, U8_TYPE_ID]);
        for func in self.functions.iter_mut() {
            for block in func.blocks.iter_mut() {
                block.reduce_to_bytes();
            }
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
        U16_TYPE_ID,
        Action::InitializeStruct(tvec![high_inv, high_middle_inv, low_middle_inv, low_inv]),
    ));
    new_steps
}

impl Block {
    fn reduce_to_bytes(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Block::reduce_sum_types");
        let mut step_id = StepId::from(0);
        while step_id.0 < self.steps.len() {
            match &mut self.steps[step_id].action {
                &mut Action::LoadConstant(ref mut obj) => {
                    obj.reduce_to_bytes();
                }
                &mut Action::UnaryOperation(op, target_id) => {
                    let new_steps = match (self.steps[target_id].ty, op) {
                        (U16_TYPE_ID, UnaryOperation::Invert) => invert_u16(),
                        (U32_TYPE_ID, UnaryOperation::Invert) => invert_u32(),
                        (_, UnaryOperation::Invert) => {
                            step_id.0 += 1;
                            continue;
                        }
                    };
                    step_id = self.insert_steps(
                        step_id..=step_id,
                        new_steps,
                        iter::once(target_id),
                    );
                }
                &mut Action::Binop(op, a, b) =>  {
                    let new_steps = match (self.steps[a].ty, self.steps[b].ty, op) {
                        _ => unimplemented!(),
                    };
                    step_id = self.insert_steps(
                        step_id..=step_id,
                        new_steps,
                        [a, b].iter().copied(),
                    );
                }
                _ => step_id.0 += 1,
            }
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
