use shared_id::{TypeId, U32_TYPE_ID, U16_TYPE_ID, U8_TYPE_ID};

use crate::{Type, Mir, StepId, Action, Block};

impl<'a> Mir<'a> {
    /// reduce all `u32` and `u16` to `u8`
    pub fn reduce_to_bytes(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::reduce_sum_types");
        self.types[U32_TYPE_ID] = Type::Struct(tvec![U8_TYPE_ID, U8_TYPE_ID, U8_TYPE_ID, U8_TYPE_ID]);
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
            let ty = self.steps[step_id].ty;
            match &self.steps[step_id].action {
                Action::LoadConstant(obj) => {
                    // TODO: split constant into bytes
                }
                Action::UnaryOperation(op, step) => {
                    // TODO: resolve unary operation
                }
                Action::Binop(op, a, b) => {
                    // TODO: resolve Binop
                }
                _ => (),
            }
            step_id.0 += 1;
        }
    }
}