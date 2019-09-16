use crate::{Action, BlockId, FunctionId, Mir, Type, TypeId};

impl Mir {
    /// check if the MIR is well formed
    pub fn validate(&self) {
        for ty in 0..self.types.len() {
            self.validate_type(TypeId(ty));
        }

        for func in 0..self.functions.len() {
            self.validate_function(FunctionId(func));
        }
    }

    pub fn validate_type(&self, ty: TypeId) {
        let _ = ty;
    }

    pub fn validate_function(&self, func: FunctionId) {
        let func = &self[func];
        for (block_id, block) in func.content.iter().enumerate() {
            for (step_id, step) in block.content.iter().enumerate() {
                match &step.action {
                    &Action::LoadInput(v) => assert_eq!(block.input[v], step.ty),
                    Action::LoadConstant(_) => (), // TODO: check type of constant
                    Action::Return(id) => {
                        assert_eq!(self[step.ty], Type::Uninhabited);
                        assert!(id.0 < step_id);
                        // TODO: check function return type
                    }
                    &Action::CallFunction(target_func, ref input) => {
                        let expected_input = &self[target_func][BlockId(0)].input;
                        assert_eq!(expected_input.len(), input.len());
                        for i in 0..expected_input.len() {
                            assert!(input[i].0 < step_id);
                            assert_eq!(expected_input[i], block[input[i]].ty);
                        }
                    }
                    &Action::FieldAccess(id, field) => {
                        assert!(id.0 < step_id);
                        if let Type::Struct(ty) = &self[block[id].ty] {
                            let field_ty = ty[field.0];
                            assert_eq!(field_ty, step.ty);
                        } else {
                            panic!("field access on invalid type: {:?}", block.step(id).ty);
                        }
                    }
                    &Action::Add(a, b)
                    | &Action::Sub(a, b)
                    | &Action::Mul(a, b)
                    | &Action::Div(a, b) => {
                        assert!(a.0 < step_id);
                        assert!(b.0 < step_id);
                        assert_eq!(block[a].ty, block[b].ty);
                        assert_eq!(block[a].ty, step.ty);
                    }
                    _ => (), // TODO: validate
                }
            }
        }
    }
}
