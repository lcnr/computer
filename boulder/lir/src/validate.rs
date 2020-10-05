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
        for step_id in block.steps.index_iter() {
            let step_panic = PanicDisplay("step: ", &step_id);
            match block.steps[step_id] {
                Action::Invert(i, o)
                | Action::BlackBox(i, o)
                | Action::Move(Arg::Location(i), o) => {
                    assert!(i.0 < func.memory_len);
                    assert!(o.0 < func.memory_len);
                }
                Action::Debug(i) => {
                    assert!(i.0 < func.memory_len);
                }
                Action::Move(Arg::Byte(_), o) => {
                    assert!(o.0 < func.memory_len);
                }
                Action::Binop { l, r, out, .. } => {
                    if let Arg::Location(id) = l {
                        assert!(id.0 < func.memory_len);
                    }
                    if let Arg::Location(id) = r {
                        assert!(id.0 < func.memory_len);
                    }
                    assert!(out.0 < func.memory_len);
                }
                Action::FunctionCall {
                    id,
                    ref args,
                    ref ret,
                } => {
                    let target = &self.functions[id];
                    check_args(func.memory_len, args);

                    for val in ret.iter().filter_map(Option::as_ref) {
                        assert!(val.0 < func.memory_len);
                    }

                    assert_eq!(target.input_len, args.len());
                    assert_eq!(target.return_len, ret.len());
                }
                Action::Noop => (),
            }
            mem::forget(step_panic);

            match block.terminator {
                Terminator::Goto(target) => {
                    if let Some(target) = target {
                        assert!(target < func.blocks.range_end());
                    }
                }
                Terminator::Match(value, ref _arms) => {
                    assert!(value.0 < func.memory_len);
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
