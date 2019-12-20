use std::{fmt, mem, ops::Drop};

use tindex::TIndex;

use shared_id::{
    FunctionId, TypeId, EMPTY_TYPE_ID, U16_BYTES_TYPE_ID, U16_TYPE_ID, U32_BYTES_TYPE_ID,
    U32_TYPE_ID,
};

use crate::{Action, Mir, Object, StepId, Terminator, Type, UnaryOperation};

struct PanicDisplay<'a, 'b>(&'a str, &'b dyn fmt::Display);

impl Drop for PanicDisplay<'_, '_> {
    fn drop(&mut self) {
        eprintln!("{}{}", self.0, self.1);
    }
}

impl<'a> Mir<'a> {
    /// check if the MIR is well formed
    pub fn validate(&self) {
        #[cfg(feature = "profiler")]
        profile_scope!("validate");
        for ty in 0..self.types.len() {
            self.validate_type(ty.into());
        }

        for func in 0..self.functions.len() {
            self.validate_function(func.into());
        }
    }

    pub fn validate_type(&self, ty: TypeId) {
        let _ = ty;
    }

    pub fn validate_function(&self, func: FunctionId) {
        #[cfg(feature = "profiler")]
        profile_scope!("validate_function");
        let hir_panic = PanicDisplay("\n", self);
        let func_panic = func.as_index();
        let func_panic = PanicDisplay("function: ", &func_panic);

        let func = &self[func];
        for (block_id, block) in func.blocks.iter().enumerate() {
            let block_panic = PanicDisplay("block: ", &block_id);
            for (step_id, step) in block.steps.iter().enumerate() {
                let step_id = StepId::from(step_id);
                let step_panic = PanicDisplay("step: ", &step_id.0);
                match &step.action {
                    &Action::Extend(s) => {
                        assert!(s < step_id);
                        if block[s].ty != step.ty {
                            if let &Type::Sum(ref v) = &self[step.ty] {
                                if let &Type::Sum(ref s) = &self[block[s].ty] {
                                    assert!(s.iter().all(|t| v.get(t)));
                                } else {
                                    assert!(v.get(block[s].ty));
                                }
                            } else {
                                unreachable!("mismatched types");
                            }
                        }
                    }
                    &Action::Reduce(s) => {
                        assert!(s < step_id);
                        if let &Type::Sum(ref v) = &self[block[s].ty] {
                            if let &Type::Sum(ref s) = &self[step.ty] {
                                assert!(s.iter().all(|t| v.get(t)));
                            } else {
                                assert!(v.get(step.ty));
                            }
                        } else {
                            unreachable!("mismatched types");
                        }
                    }
                    &Action::LoadInput(v) => assert_eq!(block.input[v], step.ty),
                    &Action::LoadConstant(ref c) => {
                        match &self.types[step.ty] {
                            &Type::Sum(ref options) => {
                                if let &Object::Variant(id, _) = c {
                                    assert!(options.get(id));
                                } else {
                                    unreachable!("sum type with non variant object");
                                }
                            }
                            _ => (), // TODO
                        }
                    }
                    &Action::InitializeStruct(ref field_values) => {
                        if let &Type::Struct(ref field_types) = &self.types[step.ty] {
                            assert_eq!(field_values.len(), field_types.len());
                            for (&v, &ty) in field_values.iter().zip(field_types.iter()) {
                                assert!(v < step_id);
                                assert_eq!(block[v].ty, ty);
                            }
                        } else {
                            unreachable!("initialize struct with non struct type");
                        }
                    }
                    &Action::InitializeUnion(id) => {
                        assert!(id < step_id);
                        if let Type::Union(ref fields) = &self[step.ty] {
                            assert!(fields.get(block[id].ty));
                        } else {
                            panic!("initialize union with invalid type: {:?}", block[id].ty);
                        }
                    }
                    &Action::CallFunction(target_func, ref input) => {
                        let expected_input = self[target_func].args();
                        assert_eq!(expected_input.len(), input.len());
                        for i in 0..expected_input.len() {
                            assert!(input[i] < step_id);
                            assert_eq!(expected_input[i], block[input[i]].ty);
                        }
                        assert_eq!(step.ty, self[target_func].ret);
                    }
                    &Action::StructFieldAccess(id, field) => {
                        assert!(id < step_id);
                        if let Type::Struct(ty) = &self[block[id].ty] {
                            let field_ty = ty[field];
                            assert_eq!(field_ty, step.ty);
                        } else {
                            panic!("field access on invalid type: {:?}", block[id].ty);
                        }
                    }
                    &Action::UnionFieldAccess(id) => {
                        assert!(id < step_id);
                        if let Type::Union(fields) = &self[block[id].ty] {
                            assert!(fields.get(step.ty));
                        } else {
                            unreachable!("union field access on invalid type: {:?}", block[id].ty);
                        }
                    }
                    &Action::UnaryOperation(kind, expr) => {
                        assert!(expr < step_id);
                        match kind {
                            UnaryOperation::Invert => {
                                assert_eq!(block[expr].ty, step.ty);
                                // TODO: validate integer or bool
                            }
                            UnaryOperation::ToBytes => {
                                assert!(
                                    block[expr].ty == U16_TYPE_ID && step.ty == U16_BYTES_TYPE_ID
                                        || block[expr].ty == U32_TYPE_ID
                                            && step.ty == U32_BYTES_TYPE_ID
                                );
                            }
                            UnaryOperation::FromBytes => {
                                assert!(
                                    block[expr].ty == U16_BYTES_TYPE_ID && step.ty == U16_TYPE_ID
                                        || block[expr].ty == U32_BYTES_TYPE_ID
                                            && step.ty == U32_TYPE_ID
                                );
                            }
                            UnaryOperation::Debug => assert_eq!(step.ty, EMPTY_TYPE_ID),
                        }
                    }
                    &Action::Binop(kind, a, b) => {
                        assert!(a < step_id);
                        assert!(b < step_id);
                        kind.validate(step, &block[a], &block[b]);
                    } // TODO: validate
                }
                mem::forget(step_panic);
            }

            match &block.terminator {
                &Terminator::Goto(target, ref steps) => {
                    if let Some(target) = target {
                        let target = &func[target];
                        assert_eq!(target.input.len(), steps.len());
                        for i in 0..steps.len() {
                            assert_eq!(target.input[i], block[steps[i]].ty);
                        }
                    } else {
                        assert_eq!(steps.len(), 1);
                        assert_eq!(func.ret, block[steps[0]].ty);
                    }
                }
                &Terminator::Match(value, ref targets) => {
                    for &(ty, target, ref args) in targets.iter() {
                        let target_input;
                        let target_input: &[_] = if let Some(target) = target {
                            &func[target].input
                        } else {
                            assert_eq!(args.len(), 1);
                            target_input = [func.ret];
                            &target_input
                        };
                        assert_eq!(target_input.len(), args.len());
                        for i in 0..args.len() {
                            if block[value].ty != ty {
                                if let &Type::Sum(ref s) = &self[block[value].ty] {
                                    if let &Type::Sum(ref v) = &self[ty] {
                                        assert!(v.iter().all(|t| s.get(t)));
                                    } else {
                                        assert!(s.get(ty));
                                    }
                                } else {
                                    unreachable!("mismatched types");
                                }
                            }
                            if let Some(arg) = args[i] {
                                assert_eq!(target_input[i], block[arg].ty)
                            } else {
                                assert_eq!(target_input[i], ty)
                            }
                        }
                    }
                }
            }
            mem::forget(block_panic);
        }

        mem::forget(func_panic);
        mem::forget(hir_panic);
    }
}
