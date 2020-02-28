#![allow(unused)]

use std::fmt;

use tindex::{bitset::TBitSet, TSlice, TVec};

use shared_id::{BlockId, FunctionId, InputId, LocationId, TagId};

use lir::{Action, Binop, Function, Lir, Terminator};

struct TagManager {
    tag: TagId,
}

impl TagManager {
    fn next(&mut self) -> TagId {
        let tag = self.tag;
        self.tag = tag + 1;
        tag
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Writeable {
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
enum Readable {
    A,
    B,
    C,
    D,
    Mem,
    Byte(u8),
    Tag(TagId),
}

impl Readable {
    fn is_mem_access(self) -> bool {
        match self {
            Readable::Mem | Readable::Byte(_) | Readable::Tag(_) => true,
            _ => false,
        }
    }

    fn size(self) -> usize {
        if let Readable::Byte(_) | Readable::Tag(_) = self {
            1
        } else {
            0
        }
    }
}

#[derive(Debug, Clone)]
enum Operation {
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
enum Cond {
    Zero(Command<Void>),
    NZero(Command<Void>),
    Eq(Command<Void>),
    Neq(Command<Void>),
    Gt(Command<Void>),
    Gte(Command<Void>),
    // TODO: consider adding Lt and Lte
}

impl Cond {
    fn inner(&self) -> &Command<Void> {
        match self {
            Cond::Zero(c)
            | Cond::NZero(c)
            | Cond::Eq(c)
            | Cond::Neq(c)
            | Cond::Gt(c)
            | Cond::Gte(c) => c,
        }
    }
}

/// Assembler instructions, we are not using `rock::Command`
/// as there are some commands we only decode later on.
#[derive(Debug, Clone)]
enum Command<T: fmt::Debug + Clone + CommandSize = Cond> {
    /// moves M1 and M2 so `mem` points to the given location.
    Comment(Box<str>),
    /// A block internal tag which will be changed into a section address during compilation.
    Tag(TagId),
    MemStorage(FunctionId, LocationId),
    Move(Readable, Writeable),
    Op(Operation, Writeable),
    /// jump to the given section.
    Jump(Readable),
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
    /// This may invalide any register.
    ///
    Return,
}

trait CommandSize {
    fn max_size(&self) -> usize;
}

impl<T: fmt::Debug + Clone + CommandSize> CommandSize for Command<T> {
    fn max_size(&self) -> usize {
        match self {
            Command::Comment(_) | Command::Tag(_) => 0,
            Command::MemStorage(_, _) => 2,
            Command::Move(r, _) | Command::Jump(r) => 1 + r.size(),
            Command::Op(_, _) => 1,
            Command::If(v) => 1 + v.max_size(),
            // mov func_block A;
            // mov func_block.func_start B;
            // ret A B;
            Command::GotoFunction(_) | Command::GotoBlock(_, _) => 5,
            // 2: mov 0xff M2;         # load stack pointer
            // 2: mov 0 M1;
            // 1: mov mem A;
            // 2: mov 2 B;             # pop return address from stack
            // 1: sub mem;
            // 1: mov A M1;
            // 1: mov mem C;           # load return block to C
            // 2: mov 1 B;             # point mem to return byte
            // 1: sub M1;
            // 1: ret C mem;           # return
            Command::Return => 14,
        }
    }
}

impl CommandSize for Cond {
    fn max_size(&self) -> usize {
        self.inner().max_size()
    }
}

impl CommandSize for Void {
    fn max_size(&self) -> usize {
        0
    }
}

impl CommandSize for Vec<Command> {
    fn max_size(&self) -> usize {
        self.iter().map(CommandSize::max_size).sum()
    }
}

/// converts `lir` to humanly readable assembler.
pub fn convert(lir: Lir) -> String {
    let mut tm = TagManager { tag: TagId(0) };

    for f in lir.functions.index_iter() {
        for b in lir.functions[f].blocks.index_iter() {
            let block = &lir.functions[f].blocks[b];
            println!(
                "{}{}.max_size(): {}",
                f,
                b,
                convert_block(&mut tm, &lir.ctx, &lir.functions, block, f).max_size()
            );
        }
    }

    String::new()
}

fn convert_block(
    tm: &mut TagManager,
    ctx: &lir::Context,
    functions: &TSlice<FunctionId, Function>,
    lir: &lir::Block,
    f: FunctionId,
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
                commands.push(Command::MemStorage(f, i));
                commands.push(Command::Move(Readable::Mem, Writeable::A));
                commands.push(Command::MemStorage(f, o));
                commands.push(Command::Op(Operation::Invert, Writeable::Mem));
            }
            Action::Move(i, o) => {
                commands.push(Command::MemStorage(f, i));
                commands.push(Command::Move(Readable::Mem, Writeable::A));
                commands.push(Command::MemStorage(f, o));
                commands.push(Command::Move(Readable::A, Writeable::Mem));
            }
            Action::Debug(_) => {
                unimplemented!("Debug to ASM");
            }
            Action::LoadConstant(v, o) => {
                commands.push(Command::Move(Readable::Byte(v), Writeable::A));
                commands.push(Command::MemStorage(f, o));
                commands.push(Command::Move(Readable::A, Writeable::Mem));
            }
            Action::Binop { op, l, r, out } => {
                commands.push(Command::MemStorage(f, l));
                commands.push(Command::Move(Readable::Mem, Writeable::A));
                commands.push(Command::MemStorage(f, r));
                commands.push(Command::Move(Readable::Mem, Writeable::B));
                commands.push(Command::MemStorage(f, out));
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
                for i in args.index_iter() {
                    let arg = args[i];
                    commands.push(Command::MemStorage(f, arg));
                    commands.push(Command::Move(Readable::Mem, Writeable::A));
                    commands.push(Command::MemStorage(id, inputs[i]));
                    commands.push(Command::Move(Readable::A, Writeable::Mem));
                }

                // TODO: recursive functions don't work right now
                // TODO: return arguments on stack
                commands.push(Command::GotoFunction(id));

                // TODO: functions currently store their return values at
                // the start of the memory storage, this could be improved
                for (i, &ret) in ret.iter().enumerate() {
                    if let Some(ret) = ret {
                        let i = LocationId(i);
                        commands.push(Command::MemStorage(id, i));
                        commands.push(Command::Move(Readable::Mem, Writeable::A));
                        commands.push(Command::MemStorage(f, ret));
                        commands.push(Command::Move(Readable::A, Writeable::Mem));
                    }
                }
            }
        }
    }

    commands.push(Command::Comment(Box::from(format!("{}", lir.terminator))));
    match lir.terminator {
        Terminator::Goto(target, ref args) => goto(&mut commands, functions, f, target, args),
        Terminator::Match(expr, ref arms) => {
            commands.push(Command::MemStorage(f, expr));
            commands.push(Command::Move(Readable::Mem, Writeable::A));
            if let Some((last, rest)) = arms.split_last() {
                for arm in rest.iter() {
                    let tag = tm.next();
                    if arm.pat == 0 {
                        commands.push(Command::If(Cond::NZero(Command::Jump(Readable::Tag(tag)))));
                    } else {
                        commands.push(Command::Move(Readable::Byte(arm.pat), Writeable::B));
                        commands.push(Command::If(Cond::Neq(Command::Jump(Readable::Tag(tag)))));
                    }
                    goto(&mut commands, functions, f, arm.target, &arm.args);
                    commands.push(Command::Tag(tag));
                }

                // we can ignore the condition of the last match arm
                // as it is UB if it would not match
                goto(&mut commands, functions, f, last.target, &last.args);
            }
        }
    }
    commands
}

fn goto(
    commands: &mut Vec<Command>,
    functions: &TSlice<FunctionId, Function>,
    f: FunctionId,
    target: Option<BlockId>,
    args: &TSlice<InputId, LocationId>,
) {
    if let Some(target) = target {
        let inputs = functions[f].blocks[target].inputs.iter().copied();
        terminator_memory(commands, f, inputs, args);
        commands.push(Command::GotoBlock(f, target));
    } else {
        terminator_memory(commands, f, (0..args.len()).map(LocationId), args);
        commands.push(Command::Return);
    }
}

/// This method does not use the A and B registers
fn terminator_memory<I>(
    commands: &mut Vec<Command>,
    f: FunctionId,
    targets: I,
    args: &TSlice<InputId, LocationId>,
) where
    I: Iterator<Item = LocationId>,
{
    let mut args: TVec<_, _> = args.to_owned();
    let mut used = TBitSet::new();
    for (id, target) in args.index_iter().zip(targets) {
        used.add(target);
        let v = args[id];
        if v != target {
            if args[id + 1..].contains(&target) {
                // target location is still needed, move
                // to an available space
                let mut free = LocationId(0);
                while used.get(free) || args[id + 1..].contains(&free) {
                    free = free + 1;
                }

                for arg in args[id + 1..].iter_mut().filter(|&&mut arg| arg == target) {
                    *arg = free;
                }

                commands.push(Command::MemStorage(f, target));
                commands.push(Command::Move(Readable::Mem, Writeable::C));
                commands.push(Command::MemStorage(f, free));
                commands.push(Command::Move(Readable::C, Writeable::Mem));
            }

            commands.push(Command::MemStorage(f, v));
            commands.push(Command::Move(Readable::Mem, Writeable::C));
            commands.push(Command::MemStorage(f, target));
            commands.push(Command::Move(Readable::C, Writeable::Mem));

            for arg in args[id + 1..].iter_mut().filter(|&&mut arg| arg == v) {
                *arg = target;
            }
        }
    }
}
