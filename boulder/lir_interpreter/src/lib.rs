#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use tindex::tvec;

use shared_id::{BlockId, FunctionId, LocationId, StepId};

use lir::{Action, Arg, Binop, Lir, Terminator};

#[derive(Debug, Clone)]
pub enum Error {
    ArgumentCount,
    ReadUndefined,
    /// integer over/underflow etc
    Arithmetic,
    StackOverflow,
    MaxSteps,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Memory {
    Byte(u8),
    Undefined,
}

impl Memory {
    fn valid(self) -> Result<u8, Error> {
        match self {
            Memory::Byte(v) => Ok(v),
            Memory::Undefined => Err(Error::ReadUndefined),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoulderLirInterpreter<'a> {
    lir: &'a Lir<'a>,
    step_count: usize,
    function: FunctionId,
    block: BlockId,
    step: StepId,
}

impl<'a> BoulderLirInterpreter<'a> {
    pub fn new(lir: &'a Lir<'a>) -> Self {
        Self {
            lir,
            step_count: 0,
            function: FunctionId(0),
            block: BlockId(0),
            step: StepId(0),
        }
    }

    pub fn last_step(&self) -> (FunctionId, BlockId, StepId) {
        (self.function, self.block, self.step)
    }

    pub fn step_count(&self) -> usize {
        self.step_count
    }

    pub fn reset_step_count(&mut self) {
        self.step_count = 0
    }

    pub fn execute_function(
        &mut self,
        id: FunctionId,
        args: &[Memory],
        stack_depth: u32,
        max_steps: usize,
    ) -> Result<Vec<Memory>, Error> {
        if stack_depth == 0 {
            return Err(Error::StackOverflow);
        }
        #[cfg(feature = "profiler")]
        profile_scope!("execute_function");
        self.function = id;
        let mut memory = tvec![Memory::Undefined; self.lir.functions[id].memory_len];
        for (&arg, mem) in args.iter().zip(memory.iter_mut()) {
            *mem = arg;
        }

        let mut block_id = BlockId::from(0);
        self.block = block_id;
        loop {
            #[cfg(feature = "profiler")]
            profile_scope!("execute_block");

            let block = &self.lir.functions[id].blocks[block_id];
            for step_id in block.steps.index_iter() {
                self.step = step_id;
                let step = &block.steps[step_id];
                #[cfg(feature = "profiler")]
                profile_scope!("execute_step");

                self.step_count += 1;
                if self.step_count >= max_steps {
                    return Err(Error::MaxSteps);
                }

                match *step {
                    Action::Invert(i, o) => memory[o] = Memory::Byte(!memory[i].valid()?),
                    Action::BlackBox(i, o) | Action::Move(Arg::Location(i), o) => {
                        memory[o] = memory[i]
                    }
                    Action::Move(Arg::Byte(v), o) => memory[o] = Memory::Byte(v),
                    Action::Debug(i) => println!(
                        "debug ({}:{}:{}): {} = {:?}",
                        id, block_id, step_id, i, memory[i]
                    ),
                    Action::Binop { op, l, r, out } => {
                        let v = |p| match p {
                            Arg::Byte(v) => Memory::Byte(v),
                            Arg::Location(id) => memory[id],
                        };
                        memory[out] = Memory::Byte(self.binop(op, v(l), v(r))?)
                    }
                    Action::FunctionCall {
                        id,
                        ref args,
                        ref ret,
                    } => {
                        let args: Vec<_> = args
                            .iter()
                            .map(|&l| match l {
                                Some(Arg::Byte(v)) => Memory::Byte(v),
                                Some(Arg::Location(location)) => memory[location],
                                None => Memory::Undefined,
                            })
                            .collect();
                        let func = self.function;
                        let values =
                            self.execute_function(id, &args, stack_depth - 1, max_steps)?;
                        self.function = func;
                        self.block = block_id;
                        self.step = step_id;
                        for (&ret, v) in ret.iter().zip(values) {
                            if let Some(adr) = ret {
                                memory[adr] = v;
                            }
                        }
                    }
                    Action::Noop => (),
                }
            }

            self.step_count += 1;
            if self.step_count >= max_steps {
                return Err(Error::MaxSteps);
            }

            match block.terminator {
                Terminator::Goto(None) => {
                    return Ok((0..self.lir.functions[id].return_len)
                        .map(|v| memory[LocationId(v)])
                        .collect())
                }
                Terminator::Goto(Some(block)) => {
                    block_id = block;
                    self.block = block_id;
                }
                Terminator::Match(expr, ref arms) => {
                    let value = memory[expr].valid()?;

                    let target = arms
                        .iter()
                        .find(|arm| arm.pat == value)
                        .unwrap_or_else(|| arms.last().unwrap())
                        .target;
                    match target {
                        None => {
                            return Ok((0..self.lir.functions[id].return_len)
                                .map(|v| memory[LocationId(v)])
                                .collect())
                        }
                        Some(block) => {
                            block_id = block;
                            self.block = block
                        }
                    }
                }
            }
        }
    }

    fn to_bool(&self, b: bool) -> Option<u8> {
        if b {
            Some(self.lir.ctx.true_replacement)
        } else {
            Some(self.lir.ctx.false_replacement)
        }
    }

    fn binop(&self, op: Binop, l: Memory, r: Memory) -> Result<u8, Error> {
        let l = l.valid()?;
        let r = r.valid()?;

        let inner = |op, l: u8, r: u8| match op {
            Binop::Add => l.checked_add(r),
            Binop::Sub => l.checked_sub(r),
            Binop::Shl => l.checked_shl(r.into()).or(Some(0)),
            Binop::Shr => l.checked_shr(r.into()).or(Some(0)),
            Binop::Eq => self.to_bool(l == r),
            Binop::Neq => self.to_bool(l != r),
            Binop::Gt => self.to_bool(l > r),
            Binop::Gte => self.to_bool(l >= r),
            Binop::BitOr => Some(l | r),
            Binop::BitAnd => Some(l & r),
            Binop::BitXor => Some(l ^ r),
        };

        if let Some(r) = inner(op, l, r) {
            Ok(r)
        } else {
            Err(Error::Arithmetic)
        }
    }
}
