use std::{fmt, mem};

use tindex::TSlice;

use shared_id::{BlockId, FunctionId, InputId};

use crate::{Action, Arg, Lir, Terminator};

struct PanicDisplay<'a, 'b>(&'a str, &'b dyn fmt::Display);

impl Drop for PanicDisplay<'_, '_> {
    fn drop(&mut self) {
        eprintln!("{}{}", self.0, self.1);
    }
}

impl<'a> Lir<'a> {
    pub fn validate(&self) {
        #[cfg(feature = "profiler")]
        profile_scope!("validate");
        for func in self.functions.index_iter() {
            self.validate_function(func)
        }
    }

    pub fn validate_function(&self, func: FunctionId) {
        #[cfg(feature = "profiler")]
        profile_scope!("validate_function");
        let lir_panic = PanicDisplay("\n", self);
        let func_panic = PanicDisplay("function: ", &func);

        for block_id in self.functions[func].blocks.index_iter() {
            self.validate_block(func, block_id)
        }

        mem::forget(func_panic);
        mem::forget(lir_panic);
    }

    #[allow(clippy::cognitive_complexity)]
    fn validate_block(&self, func_id: FunctionId, block_id: BlockId) {
        let block_panic = PanicDisplay("block: ", &block_id);

        let func = &self.functions[func_id];
        let block = &func.blocks[block_id];
        for input in block.inputs.iter() {
            assert!(input.0 < block.memory_len);
        }

        for step_id in block.steps.index_iter() {
            let step_panic = PanicDisplay("step: ", &step_id);
            match block.steps[step_id] {
                Action::Invert(i, o) | Action::Move(i, o) => {
                    assert!(i.0 < block.memory_len);
                    assert!(o.0 < block.memory_len);
                }
                Action::Debug(i) => {
                    assert!(i.0 < block.memory_len);
                }
                Action::LoadConstant(_, o) => {
                    assert!(o.0 < block.memory_len);
                }
                Action::Binop { l, r, out, .. } => {
                    if let Arg::Location(id) = l {
                        assert!(id.0 < block.memory_len);
                    }
                    if let Arg::Location(id) = r {
                        assert!(id.0 < block.memory_len);
                    }
                    assert!(out.0 < block.memory_len);
                }
                Action::FunctionCall {
                    id,
                    ref args,
                    ref ret,
                } => {
                    let target = &self.functions[id];
                    check_args(block.memory_len, args);

                    for val in ret.iter().filter_map(Option::as_ref) {
                        assert!(val.0 < block.memory_len);
                    }

                    assert_eq!(target.input_len(), args.len());
                    assert_eq!(target.return_length, ret.len());
                }
            }
            mem::forget(step_panic);

            match block.terminator {
                Terminator::Goto(target, ref args) => {
                    if let Some(target) = target {
                        assert_eq!(func.blocks[target].inputs.len(), args.len());
                    } else {
                        assert_eq!(func.return_length, args.len());
                    }

                    check_args(block.memory_len, args);
                }
                Terminator::Match(value, ref arms) => {
                    assert!(value.0 < block.memory_len);
                    for arm in arms.iter() {
                        if let Some(target) = arm.target {
                            assert_eq!(func.blocks[target].inputs.len(), arm.args.len());
                        } else {
                            assert_eq!(func.return_length, arm.args.len());
                        }

                        check_args(block.memory_len, &arm.args);
                    }
                }
            }
        }

        mem::forget(block_panic);
    }
}

fn check_args(len: usize, args: &TSlice<InputId, Option<Arg>>) {
    for &arg in args.iter() {
        if let Some(Arg::Location(id)) = arg {
            assert!(id.0 < len);
        }
    }
}
