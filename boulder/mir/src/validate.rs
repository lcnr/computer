use std::{fmt, mem, ops::Drop};

use shared_id::{
    BlockId, FunctionId, StepId, TypeId, EMPTY_TYPE_ID, U16_BYTES_TYPE_ID, U16_TYPE_ID,
    U32_BYTES_TYPE_ID, U32_TYPE_ID, U8_TYPE_ID,
};

use crate::{Action, Block, Function, MatchArm, Mir, Object, Terminator, Type, UnaryOperation};

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
        for ty in self.types.index_iter() {
            self.validate_type(ty);
        }

        for func in self.functions.index_iter() {
            self.validate_function(func);
        }
    }

    pub fn validate_type(&self, ty: TypeId) {
        let _ = ty;
    }

    pub fn validate_function(&self, func: FunctionId) {
        #[cfg(feature = "profiler")]
        profile_scope!("validate_function");
        let hir_panic = PanicDisplay("\n", self);
        let func_panic = PanicDisplay("function: ", &func);

        for block_id in self[func].blocks.index_iter() {
            self.validate_block(func, block_id, &self.functions[func].blocks[block_id])
        }

        mem::forget(func_panic);
        mem::forget(hir_panic);
    }

    fn validate_block(&self, func: FunctionId, block_id: BlockId, block: &Block) {
        let func = &self.functions[func];
        let block_panic = PanicDisplay("block: ", &block_id);
        for (step_id, step) in block.steps.iter().enumerate() {
            let step_id = StepId(step_id);
            let step_panic = PanicDisplay("step: ", &step_id);
            match step.action {
                Action::Extend(s) => {
                    assert!(!self.ctx.e2b);
                    assert!(s < step_id);
                    if block.steps[s].ty != step.ty {
                        if let Type::Sum(ref v) = self[step.ty] {
                            if let Type::Sum(ref s) = self[block.steps[s].ty] {
                                assert!(s.iter().all(|t| v.get(t)));
                            } else {
                                assert!(v.get(block.steps[s].ty));
                            }
                        } else {
                            unreachable!("mismatched types");
                        }
                    }
                }
                Action::Reduce(s) => {
                    assert!(s < step_id);
                    if let Type::Sum(ref v) = self[block.steps[s].ty] {
                        if let Type::Sum(ref s) = self[step.ty] {
                            assert!(s.iter().all(|t| v.get(t)));
                        } else {
                            assert!(v.get(step.ty));
                        }
                    } else {
                        unreachable!("mismatched types");
                    }
                }
                Action::LoadInput(v) => assert_eq!(block.input[v], step.ty),
                Action::LoadConstant(ref c) => {
                    if let Type::Sum(ref options) = self.types[step.ty] {
                        if let Object::Variant(id, _) = *c {
                            assert!(options.get(id));
                        } else {
                            unreachable!("sum type with non variant object");
                        }
                    }
                }
                Action::InitializeStruct(ref field_values) => {
                    if let Type::Struct(ref field_types) = self.types[step.ty] {
                        assert_eq!(field_values.len(), field_types.len());
                        for (&v, &ty) in field_values.iter().zip(field_types.iter()) {
                            assert!(v < step_id);
                            assert_eq!(block.steps[v].ty, ty);
                        }
                    } else {
                        unreachable!("initialize struct with non struct type");
                    }
                }
                Action::InitializeUnion(id) => {
                    assert!(id < step_id);
                    if let Type::Union(ref fields) = &self[step.ty] {
                        assert!(fields.get(block.steps[id].ty));
                    } else {
                        panic!(
                            "initialize union with invalid type: {:?}",
                            block.steps[id].ty
                        );
                    }
                }
                Action::CallFunction(target_func, ref input) => {
                    let expected_input = self[target_func].args();
                    assert_eq!(expected_input.len(), input.len());
                    for i in 0..expected_input.len() {
                        assert!(input[i] < step_id);
                        assert_eq!(expected_input[i], block.steps[input[i]].ty);
                    }
                    assert_eq!(step.ty, self[target_func].ret);
                }
                Action::StructFieldAccess(id, field) => {
                    assert!(id < step_id);
                    if let Type::Struct(ty) = &self[block.steps[id].ty] {
                        let field_ty = ty[field];
                        assert_eq!(field_ty, step.ty);
                    } else {
                        panic!("field access on invalid type: {:?}", block.steps[id].ty);
                    }
                }
                Action::UnionFieldAccess(id) => {
                    assert!(id < step_id);
                    if let Type::Union(fields) = &self[block.steps[id].ty] {
                        assert!(fields.get(step.ty));
                    } else {
                        unreachable!(
                            "union field access on invalid type: {:?}",
                            block.steps[id].ty
                        );
                    }
                }
                Action::UnaryOperation(kind, expr) => {
                    assert!(expr < step_id);
                    match kind {
                        UnaryOperation::Invert => {
                            assert_eq!(block.steps[expr].ty, step.ty);
                            // TODO: validate integer or bool
                        }
                        UnaryOperation::ToBytes => {
                            assert!(
                                block.steps[expr].ty == U16_TYPE_ID && step.ty == U16_BYTES_TYPE_ID
                                    || block.steps[expr].ty == U32_TYPE_ID
                                        && step.ty == U32_BYTES_TYPE_ID
                            );
                        }
                        UnaryOperation::FromBytes => {
                            assert!(
                                block.steps[expr].ty == U16_BYTES_TYPE_ID && step.ty == U16_TYPE_ID
                                    || block.steps[expr].ty == U32_BYTES_TYPE_ID
                                        && step.ty == U32_TYPE_ID
                            );
                        }
                        UnaryOperation::Debug => {
                            if !self.ctx.e2b {
                                assert_eq!(step.ty, EMPTY_TYPE_ID);
                            }
                        }
                    }
                }
                Action::Binop(kind, a, b) => {
                    assert!(a < step_id);
                    assert!(b < step_id);
                    kind.validate(step, &block.steps[a], &block.steps[b], self.ctx.e2b);
                } // TODO: validate
            }
            mem::forget(step_panic);
        }

        self.validate_terminator(func, block);

        mem::forget(block_panic);
    }

    fn validate_terminator(&self, func: &Function, block: &Block) {
        match block.terminator {
            Terminator::Goto(target, ref steps) => {
                if let Some(target) = target {
                    let target = &func[target];
                    assert_eq!(target.input.len(), steps.len());
                    for (i, &step) in steps.iter().enumerate() {
                        assert_eq!(target.input[i], block.steps[step].ty);
                    }
                } else {
                    assert_eq!(steps.len(), 1);
                    assert_eq!(func.ret, block.steps[steps[0]].ty);
                }
            }
            Terminator::Match(value, ref targets) => {
                for &MatchArm {
                    pat,
                    target,
                    ref args,
                } in targets.iter()
                {
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
                        if block.steps[value].ty != pat {
                            if let Type::Sum(ref s) = self[block.steps[value].ty] {
                                if let Type::Sum(ref v) = self[pat] {
                                    assert!(v.iter().all(|t| s.get(t)));
                                } else {
                                    assert!(s.get(pat));
                                }
                            } else {
                                unreachable!("mismatched types");
                            }
                        }
                        if let Some(arg) = args[i] {
                            assert_eq!(target_input[i], block.steps[arg].ty)
                        } else {
                            assert_eq!(target_input[i], pat)
                        }
                    }
                }
            }
            Terminator::MatchByte(value, ref arms) => {
                assert_eq!(block.steps[value].ty, U8_TYPE_ID);
                for &MatchArm {
                    target, ref args, ..
                } in arms.iter()
                {
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
                        if let Some(arg) = args[i] {
                            assert_eq!(target_input[i], block.steps[arg].ty)
                        } else {
                            assert_eq!(target_input[i], U8_TYPE_ID)
                        }
                    }
                }
            }
        }
    }
}
