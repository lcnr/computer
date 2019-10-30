use std::iter;

use tindex::TVec;

use shared_id::{TypeId, U16_TYPE_ID, U32_TYPE_ID, U8_TYPE_ID};

use crate::{Action, Block, FieldId, Mir, Object, Step, StepId, Type, UnaryOperation};

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
                &mut Action::UnaryOperation(op, target_id) => match self.steps[target_id].ty {
                    U16_TYPE_ID => match op {
                        UnaryOperation::Invert => {
                            let mut new_steps = TVec::new();
                            let high = new_steps.push(Step::new(
                                U8_TYPE_ID,
                                Action::StructFieldAccess(StepId::invalid(), FieldId::from(0)),
                            ));
                            let high_inv = new_steps.push(Step::new(
                                U8_TYPE_ID,
                                Action::UnaryOperation(UnaryOperation::Invert, high),
                            ));
                            let low = new_steps.push(Step::new(
                                U8_TYPE_ID,
                                Action::StructFieldAccess(StepId::invalid(), FieldId::from(1)),
                            ));
                            let low_inv = new_steps.push(Step::new(
                                U8_TYPE_ID,
                                Action::UnaryOperation(UnaryOperation::Invert, low),
                            ));
                            new_steps.push(Step::new(
                                U16_TYPE_ID,
                                Action::InitializeStruct(tvec![high_inv, low_inv]),
                            ));
                            step_id = self.insert_steps(
                                step_id..=step_id,
                                new_steps,
                                iter::once((StepId::invalid(), target_id)),
                            );
                        }
                    },
                    U32_TYPE_ID => {}
                    _ => (),
                },
                Action::Binop(op, a, b) => {
                    // TODO: resolve Binop
                }
                _ => (),
            }
            step_id.0 += 1;
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
