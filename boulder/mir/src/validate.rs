use std::{fmt, mem, ops::Drop};

use crate::{Action, BlockId, FunctionId, Mir, Type, TypeId, ty::*};

struct PanicDisplay<'a>(&'a str, &'a dyn fmt::Display);

impl Drop for PanicDisplay<'_> {
    fn drop(&mut self) {
        eprintln!("{}{}", self.0, self.1);
    }
}

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
        let hir_panic = PanicDisplay("\n", self);
        let func_panic = PanicDisplay("function: ", &func.0);

        let func = &self[func];
        for (block_id, block) in func.content.iter().enumerate() {
            let block_panic = PanicDisplay("block: ", &block_id);
            for (step_id, step) in block.content.iter().enumerate() {
                let step_panic = PanicDisplay("step: ", &step_id);
                match &step.action {
                    &Action::Extend(s) => {
                        assert!(s.0 < step_id);
                        if block[s].ty != step.ty {
                            if let &Type::Sum(ref v) = &self[step.ty] {
                                if let &Type::Sum(ref s) = &self[block[s].ty] {
                                    assert!(s.iter().all(|t| v.contains(t)));
                                } else {
                                    assert!(v.contains(&block[s].ty));
                                }
                            }
                        }
                    }
                    &Action::LoadInput(v) => assert_eq!(block.input[v], step.ty),
                    &Action::LoadConstant(_) => (), // TODO: check type of constant
                    &Action::Return(id) => {
                        assert_eq!(UNINHABITED_TYPE_ID, step.ty);
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
                            panic!("field access on invalid type: {:?}", block[id].ty);
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
                        // TODO: check if values are integers
                    }
                    &Action::Lt(a, b) => {
                        assert!(a.0 < step_id);
                        assert!(b.0 < step_id);
                        assert_eq!(block[a].ty, block[b].ty);
                        // TODO: check if values are integers
                        assert_eq!(BOOL_TYPE_ID, step.ty);
                    }
                    &Action::Goto(target, ref steps) => {
                        let target = &func[target];
                        assert_eq!(target.input.len(), steps.len());
                        for i in 0..steps.len() {
                            assert_eq!(target.input[i], block[steps[i]].ty);
                        }
                    }
                    _ => (), // TODO: validate
                }
                mem::forget(step_panic);
            }
            mem::forget(block_panic);
        }

        mem::forget(func_panic);
        mem::forget(hir_panic);
    }
}
