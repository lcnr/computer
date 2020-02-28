use std::fmt;

use tindex::{TSlice, TVec};

use shared_id::{BlockId, FunctionId, LocationId};

use lir::{Action, Binop, Function, Lir, Terminator};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Writeable {
    A,
    B,
    C,
    D,
    Mem,
    SectionAddr,
    BlockAddr,
}

impl Writeable {
    pub fn is_mem_access(self) -> bool {
        self == Writeable::Mem
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Readable {
    A,
    B,
    C,
    D,
    Mem,
    Byte(u8),
}

impl Readable {
    pub fn is_mem_access(self) -> bool {
        match self {
            Readable::Mem | Readable::Byte(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    Invert,
    Add,
    Sub,
    Shl,
    Shr,
    BitOr,
    BitAnd,
    BitXor,
}

#[derive(Debug, Clone)]
pub enum Void {}

/// A condition of a `Command::If`, used to remove the need for a box.
#[derive(Debug, Clone)]
pub enum Cond {
    Eq(Command<Void>),
    Neq(Command<Void>),
    Gt(Command<Void>),
    Gte(Command<Void>),
    // TODO: consider adding Lt and Lte
}

/// Assembler instructions, we are not using `rock::Command`
/// as there are some commands we only decode later on.
#[derive(Debug, Clone)]
pub enum Command<T: fmt::Debug + Clone = Cond> {
    /// moves M1 and M2 so `mem` points to the given location.
    Comment(Box<str>),
    MemStorage(FunctionId, LocationId),
    Move(Readable, Writeable),
    Op(Operation, Writeable),
    If(T),
    /// Jump to the start of a function.
    ///
    /// Both this command, as well as the given function,
    /// may invalidate any register.
    GotoFunction(FunctionId),
    /// Jump to the start of a block.
    ///
    /// Both this command, as well as the given block,
    /// may invalidate any register.
    GotoBlock(FunctionId, BlockId),
    Jump(Readable),
    LongJump(Readable),
    Return(Readable, Readable),
}

/// converts `lir` to humanly readable assembler.
pub fn convert(_lir: Lir) -> String {
    todo!()
}

pub fn convert_block(
    ctx: &lir::Context,
    functions: &TSlice<FunctionId, Function>,
    lir: &lir::Block,
    func_id: FunctionId,
) -> Vec<Command> {
    let mut commands = Vec::new();

    for step_id in lir.steps.index_iter() {
        let step = &lir.steps[step_id];
        commands.push(Command::Comment(Box::from(format!(
            "{} := {}",
            step_id, step
        ))));
        match *step {
            Action::Invert(i, o) => {
                commands.push(Command::MemStorage(func_id, i));
                commands.push(Command::Move(Readable::Mem, Writeable::A));
                commands.push(Command::MemStorage(func_id, o));
                commands.push(Command::Op(Operation::Invert, Writeable::Mem));
            }
            Action::Move(i, o) => {
                commands.push(Command::MemStorage(func_id, i));
                commands.push(Command::Move(Readable::Mem, Writeable::A));
                commands.push(Command::MemStorage(func_id, o));
                commands.push(Command::Move(Readable::A, Writeable::Mem));
            }
            Action::Debug(_) => {
                unimplemented!("Debug to ASM");
            }
            Action::LoadConstant(v, o) => {
                commands.push(Command::Move(Readable::Byte(v), Writeable::A));
                commands.push(Command::MemStorage(func_id, o));
                commands.push(Command::Move(Readable::A, Writeable::Mem));
            }
            Action::Binop { op, l, r, out } => {
                commands.push(Command::MemStorage(func_id, l));
                commands.push(Command::Move(Readable::Mem, Writeable::A));
                commands.push(Command::MemStorage(func_id, r));
                commands.push(Command::Move(Readable::Mem, Writeable::B));
                commands.push(Command::MemStorage(func_id, out));
                match op {
                    Binop::Add => commands.push(Command::Op(Operation::Add, Writeable::Mem)),
                    Binop::Sub => commands.push(Command::Op(Operation::Sub, Writeable::Mem)),
                    Binop::Shl => commands.push(Command::Op(Operation::Shl, Writeable::Mem)),
                    Binop::Shr => commands.push(Command::Op(Operation::Shr, Writeable::Mem)),
                    Binop::BitAnd => commands.push(Command::Op(Operation::BitAnd, Writeable::Mem)),
                    Binop::BitOr => commands.push(Command::Op(Operation::BitOr, Writeable::Mem)),
                    Binop::BitXor => commands.push(Command::Op(Operation::BitXor, Writeable::Mem)),
                    Binop::Eq | Binop::Neq | Binop::Gt | Binop::Gte => {
                        let cond = |op: Binop, cmd| match op {
                            Binop::Eq => Cond::Eq(cmd),
                            Binop::Neq => Cond::Neq(cmd),
                            Binop::Gt => Cond::Gt(cmd),
                            Binop::Gte => Cond::Gte(cmd),
                            _ => unreachable!(),
                        };

                        commands.push(Command::Move(
                            Readable::Byte(ctx.false_replacement),
                            Writeable::C,
                        ));
                        commands.push(Command::If(cond(
                            op,
                            Command::Move(Readable::Byte(ctx.true_replacement), Writeable::C),
                        )));
                        commands.push(Command::Move(Readable::C, Writeable::Mem));
                    }
                }
            }
            Action::FunctionCall {
                id,
                ref args,
                ref ret,
            } => {
                let other = &functions[id];

                let inputs = &other.blocks[BlockId(0)].inputs;

                // TODO: consider batching the argument passing
                for (i, &arg) in args.iter().enumerate() {
                    commands.push(Command::MemStorage(func_id, arg));
                    commands.push(Command::Move(Readable::Mem, Writeable::A));
                    commands.push(Command::MemStorage(id, inputs[i]));
                    commands.push(Command::Move(Readable::A, Writeable::Mem));
                }

                commands.push(Command::GotoFunction(id));

                // TODO: functions currently store their return values at
                // the start of the memory storage, this could be improved
                for (i, &ret) in ret.iter().enumerate() {
                    let i = LocationId(i);
                    commands.push(Command::MemStorage(id, i));
                    commands.push(Command::Move(Readable::Mem, Writeable::A));
                    commands.push(Command::MemStorage(func_id, ret));
                    commands.push(Command::Move(Readable::A, Writeable::Mem));
                }
            }
        }
    }

    match lir.terminator {
        Terminator::Goto(target, ref args) => {
            if let Some(target) = target {
                let inputs = functions[func_id].blocks[target].inputs.iter().copied();
                terminator_memory(&mut commands, func_id, inputs, args);
            } else {
                terminator_memory(
                    &mut commands,
                    func_id,
                    (0..args.len()).map(LocationId),
                    args,
                );
            }
        }
        _ => unimplemented!(),
    }
    commands
}

/// TODO: this is broken af
fn terminator_memory<I>(
    commands: &mut Vec<Command>,
    func_id: FunctionId,
    mut inputs: I,
    args: &[LocationId],
) where
    I: Iterator<Item = LocationId>,
{
    let mut args_view = args.to_vec();
    let mut args_view: &mut [_] = &mut args_view;
    for to in inputs.next() {
        let from = args_view[0];
        args_view = &mut args_view[1..];
        for arg in args_view.iter_mut() {
            if *arg == to {
                *arg = from;
            } else if *arg == from {
                *arg = to;
            }
        }
        commands.push(Command::MemStorage(func_id, from));
        commands.push(Command::Move(Readable::Mem, Writeable::A));
        commands.push(Command::MemStorage(func_id, to));
        commands.push(Command::Move(Readable::Mem, Writeable::B));
        commands.push(Command::Move(Readable::A, Writeable::Mem));
        commands.push(Command::MemStorage(func_id, from));
        commands.push(Command::Move(Readable::B, Writeable::Mem));
    }
}
