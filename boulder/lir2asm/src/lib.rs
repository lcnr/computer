#![allow(unused)]

use std::{cmp, fmt, iter};

use tindex::{TBitSet, TSlice, TVec};

use shared_id::{BlockId, FunctionId, InputId, LocationId, TagId};

use lir::{Action, Arg, Binop, Function, Lir, Terminator};

mod ctx;

use ctx::{Context, FunctionData};

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
    Block(TagId),
    Section(TagId),
}

impl Readable {
    fn is_mem_access(self) -> bool {
        match self {
            Readable::Mem | Readable::Byte(_) | Readable::Block(_) | Readable::Section(_) => true,
            _ => false,
        }
    }

    fn size(self) -> usize {
        match self {
            Readable::Byte(_) | Readable::Block(_) | Readable::Section(_) => 1,
            _ => 0,
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
    Return(Readable, Readable),
}

trait CommandSize {
    fn max_size(&self) -> usize;
}

impl<T: fmt::Debug + Clone + CommandSize> CommandSize for Command<T> {
    fn max_size(&self) -> usize {
        match self {
            Command::Comment(_) | Command::Tag(_) => 0,
            Command::Move(r, _) | Command::Jump(r) => 1 + r.size(),
            Command::Op(_, _) => 1,
            Command::If(v) => 1 + v.max_size(),
            // mov func_block A;
            // mov func_block.func_start B;
            // ret A B;
            Command::GotoFunction(_) => 5,
            Command::Return(b, s) => 1 + b.size() + s.size(),
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
    let mut ctx = Context::new();

    let storage: TVec<FunctionId, FunctionData> = lir
        .functions
        .iter()
        .map(|f| FunctionData {
            blocks: iter::repeat_with(|| ctx.tm.next())
                .take(f.blocks.len())
                .collect(),
            storage: iter::repeat_with(|| ctx.tm.next())
                .take(cmp::max(
                    f.return_length,
                    f.blocks.iter().map(|b| b.memory_len).max().unwrap(),
                ))
                .collect(),
        })
        .collect();

    let mut total = 0;
    for f in lir.functions.index_iter() {
        let mut blocks = TVec::new();
        for b in lir.functions[f].blocks.index_iter() {
            blocks.push(convert_block(&mut ctx, &storage, &lir, f, b));
        }

        if !lir.functions[f].ctx.hidden {
            println!("{}[{}]:", lir.functions[f].name, f);
            for b in lir.functions[f].blocks.index_iter() {
                println!("  {}.max_size(): {}", b, blocks[b].max_size());
            }
        }
    }

    String::new()
}

fn convert_block(
    ctx: &mut Context,
    data: &TSlice<FunctionId, FunctionData>,
    lir: &Lir,
    f: FunctionId,
    b: BlockId,
) -> Vec<Command> {
    let mut commands = Vec::new();

    let block = &lir.functions[f].blocks[b];
    for s in block.steps.index_iter() {
        let step = &block.steps[s];
        commands.push(Command::Comment(Box::from(format!("{} := {}", s, step))));
        match *step {
            Action::Invert(i, o) => {
                commands.push(Command::Move(
                    Readable::Block(data[f].storage[i]),
                    Writeable::BlockAddr,
                ));
                commands.push(Command::Move(
                    Readable::Section(data[f].storage[i]),
                    Writeable::SectionAddr,
                ));
                commands.push(Command::Move(Readable::Mem, Writeable::A));
                commands.push(Command::Move(
                    Readable::Block(data[f].storage[o]),
                    Writeable::BlockAddr,
                ));
                commands.push(Command::Move(
                    Readable::Section(data[f].storage[o]),
                    Writeable::SectionAddr,
                ));
                commands.push(Command::Op(Operation::Invert, Writeable::Mem));
            }
            Action::Move(i, o) => {
                commands.push(Command::Move(
                    Readable::Block(data[f].storage[i]),
                    Writeable::BlockAddr,
                ));
                commands.push(Command::Move(
                    Readable::Section(data[f].storage[i]),
                    Writeable::SectionAddr,
                ));
                commands.push(Command::Move(Readable::Mem, Writeable::A));
                commands.push(Command::Move(
                    Readable::Block(data[f].storage[o]),
                    Writeable::BlockAddr,
                ));
                commands.push(Command::Move(
                    Readable::Section(data[f].storage[o]),
                    Writeable::SectionAddr,
                ));
                commands.push(Command::Move(Readable::A, Writeable::Mem));
            }
            Action::Debug(_) => {
                unimplemented!("Debug to ASM");
            }
            Action::LoadConstant(v, o) => {
                commands.push(Command::Move(Readable::Byte(v), Writeable::A));
                commands.push(Command::Move(
                    Readable::Block(data[f].storage[o]),
                    Writeable::BlockAddr,
                ));
                commands.push(Command::Move(
                    Readable::Section(data[f].storage[o]),
                    Writeable::SectionAddr,
                ));
                commands.push(Command::Move(Readable::A, Writeable::Mem));
            }
            Action::Binop { op, l, r, out } => {
                match l {
                    Arg::Byte(v) => commands.push(Command::Move(Readable::Byte(v), Writeable::A)),
                    Arg::Location(id) => {
                        commands.push(Command::Move(
                            Readable::Block(data[f].storage[id]),
                            Writeable::BlockAddr,
                        ));
                        commands.push(Command::Move(
                            Readable::Section(data[f].storage[id]),
                            Writeable::SectionAddr,
                        ));
                        commands.push(Command::Move(Readable::Mem, Writeable::A));
                    }
                }

                match r {
                    Arg::Byte(v) => commands.push(Command::Move(Readable::Byte(v), Writeable::B)),
                    Arg::Location(id) => {
                        commands.push(Command::Move(
                            Readable::Block(data[f].storage[id]),
                            Writeable::BlockAddr,
                        ));
                        commands.push(Command::Move(
                            Readable::Section(data[f].storage[id]),
                            Writeable::SectionAddr,
                        ));
                        commands.push(Command::Move(Readable::Mem, Writeable::B));
                    }
                }

                commands.push(Command::Move(
                    Readable::Block(data[f].storage[out]),
                    Writeable::BlockAddr,
                ));
                commands.push(Command::Move(
                    Readable::Section(data[f].storage[out]),
                    Writeable::SectionAddr,
                ));
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
                            Readable::Byte(lir.ctx.false_replacement),
                            Writeable::C,
                        ));
                        commands.push(Command::If(cond(
                            op,
                            Command::Move(Readable::Byte(lir.ctx.true_replacement), Writeable::C),
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
                let temporaries = if lir.may_recurse(f, b, s) {
                    block.used_locations(s)
                } else {
                    TBitSet::new()
                };

                // get stack ptr
                commands.push(Command::Move(
                    Readable::Block(ctx.stack),
                    Writeable::BlockAddr,
                ));
                commands.push(Command::Move(Readable::Byte(0), Writeable::SectionAddr));

                commands.push(Command::Move(Readable::Mem, Writeable::A));
                commands.push(Command::Move(Readable::Byte(1), Writeable::B));

                // put temporaries on the stack
                if !temporaries.is_empty() {
                    for t in temporaries.iter() {
                        // load location
                        commands.push(Command::Move(
                            Readable::Block(data[f].storage[t]),
                            Writeable::BlockAddr,
                        ));
                        commands.push(Command::Move(
                            Readable::Section(data[f].storage[t]),
                            Writeable::SectionAddr,
                        ));
                        commands.push(Command::Move(Readable::Mem, Writeable::C));

                        // increment stack head
                        commands.push(Command::Op(Operation::Add, Writeable::A));
                        commands.push(Command::Move(Readable::A, Writeable::SectionAddr));
                        commands.push(Command::Move(
                            Readable::Block(ctx.stack),
                            Writeable::BlockAddr,
                        ));

                        // save location
                        commands.push(Command::Move(Readable::C, Writeable::Mem));
                    }
                }

                // put return address on the stack
                let return_adr = ctx.tm.next();
                commands.push(Command::Op(Operation::Add, Writeable::A));
                commands.push(Command::Move(Readable::A, Writeable::SectionAddr));
                commands.push(Command::Move(Readable::Block(return_adr), Writeable::C));

                commands.push(Command::Op(Operation::Add, Writeable::A));
                commands.push(Command::Move(Readable::A, Writeable::SectionAddr));
                commands.push(Command::Move(Readable::Section(return_adr), Writeable::D));

                // update stack ptr
                commands.push(Command::Move(Readable::Byte(0), Writeable::SectionAddr));
                commands.push(Command::Move(Readable::A, Writeable::Mem));

                if id == f {
                    goto(
                        &mut commands,
                        ctx,
                        data,
                        &lir.functions,
                        f,
                        Some(BlockId(0)),
                        args,
                    );
                } else {
                    let other = &lir.functions[id];
                    let inputs = &other.blocks[BlockId(0)].inputs;

                    // TODO: consider batching the argument passing
                    for i in args.index_iter() {
                        let arg = args[i];
                        match arg {
                            None => (),
                            Some(Arg::Byte(v)) => {
                                commands.push(Command::Move(Readable::Byte(v), Writeable::A));
                                commands.push(Command::Move(
                                    Readable::Block(data[id].storage[inputs[i]]),
                                    Writeable::BlockAddr,
                                ));
                                commands.push(Command::Move(
                                    Readable::Section(data[id].storage[inputs[i]]),
                                    Writeable::SectionAddr,
                                ));
                                commands.push(Command::Move(Readable::A, Writeable::Mem));
                            }
                            Some(Arg::Location(location)) => {
                                commands.push(Command::Move(
                                    Readable::Block(data[f].storage[location]),
                                    Writeable::BlockAddr,
                                ));
                                commands.push(Command::Move(
                                    Readable::Section(data[f].storage[location]),
                                    Writeable::SectionAddr,
                                ));
                                commands.push(Command::Move(Readable::Mem, Writeable::A));
                                commands.push(Command::Move(
                                    Readable::Block(data[id].storage[inputs[i]]),
                                    Writeable::BlockAddr,
                                ));
                                commands.push(Command::Move(
                                    Readable::Section(data[id].storage[inputs[i]]),
                                    Writeable::SectionAddr,
                                ));
                                commands.push(Command::Move(Readable::A, Writeable::Mem));
                            }
                        }
                    }

                    commands.push(Command::GotoFunction(id));
                }

                commands.push(Command::Tag(return_adr));

                // load return arguments
                // TODO: functions currently store their return values at
                // the start of the memory storage, this could be improved
                for (i, &ret) in ret.iter().enumerate() {
                    if let Some(ret) = ret {
                        let i = LocationId(i);

                        commands.push(Command::Move(
                            Readable::Block(data[id].storage[i]),
                            Writeable::BlockAddr,
                        ));
                        commands.push(Command::Move(
                            Readable::Section(data[id].storage[i]),
                            Writeable::SectionAddr,
                        ));
                        commands.push(Command::Move(Readable::Mem, Writeable::A));
                        commands.push(Command::Move(
                            Readable::Block(data[f].storage[ret]),
                            Writeable::BlockAddr,
                        ));
                        commands.push(Command::Move(
                            Readable::Section(data[f].storage[ret]),
                            Writeable::SectionAddr,
                        ));
                        commands.push(Command::Move(Readable::A, Writeable::Mem));
                    }
                }

                // load temporaries
                // the return address was already popped
                // put temporaries on the stack
                if !temporaries.is_empty() {
                    commands.push(Command::Move(Readable::Mem, Writeable::A));
                    commands.push(Command::Move(Readable::Byte(1), Writeable::B));

                    // TODO: consider batching temporaries
                    for t in temporaries.iter().rev() {
                        // load temp
                        commands.push(Command::Move(
                            Readable::Block(ctx.stack),
                            Writeable::BlockAddr,
                        ));
                        commands.push(Command::Move(Readable::A, Writeable::SectionAddr));
                        commands.push(Command::Move(Readable::Mem, Writeable::C));

                        // store temp in location
                        commands.push(Command::Move(
                            Readable::Block(data[f].storage[t]),
                            Writeable::BlockAddr,
                        ));
                        commands.push(Command::Move(
                            Readable::Section(data[f].storage[t]),
                            Writeable::SectionAddr,
                        ));
                        commands.push(Command::Move(Readable::C, Writeable::Mem));

                        // decrement stack ptr
                        commands.push(Command::Op(Operation::Sub, Writeable::A));
                    }

                    // update stack ptr
                    commands.push(Command::Move(
                        Readable::Block(ctx.stack),
                        Writeable::BlockAddr,
                    ));
                    commands.push(Command::Move(Readable::Byte(0), Writeable::SectionAddr));
                    commands.push(Command::Move(Readable::A, Writeable::Mem));
                }
            }
        }
    }

    commands.push(Command::Comment(Box::from(format!("{}", block.terminator))));
    match block.terminator {
        Terminator::Goto(target, ref args) => {
            goto(&mut commands, ctx, data, &lir.functions, f, target, args)
        }
        Terminator::Match(expr, ref arms) => {
            commands.push(Command::Move(
                Readable::Block(data[f].storage[expr]),
                Writeable::BlockAddr,
            ));
            commands.push(Command::Move(
                Readable::Section(data[f].storage[expr]),
                Writeable::SectionAddr,
            ));
            commands.push(Command::Move(Readable::Mem, Writeable::A));
            if let Some((last, rest)) = arms.split_last() {
                for arm in rest.iter() {
                    let tag = ctx.tm.next();
                    commands.push(Command::Move(Readable::Block(tag), Writeable::C));
                    if arm.pat == 0 {
                        commands.push(Command::If(Cond::NZero(Command::Return(
                            Readable::C,
                            Readable::Section(tag),
                        ))));
                    } else {
                        commands.push(Command::Move(Readable::Byte(arm.pat), Writeable::B));
                        commands.push(Command::If(Cond::Neq(Command::Return(
                            Readable::C,
                            Readable::Section(tag),
                        ))));
                    }
                    goto(
                        &mut commands,
                        ctx,
                        data,
                        &lir.functions,
                        f,
                        arm.target,
                        &arm.args,
                    );
                    commands.push(Command::Tag(tag));
                }

                // we can ignore the condition of the last match arm
                // as it would be UB if it did not match
                goto(
                    &mut commands,
                    ctx,
                    data,
                    &lir.functions,
                    f,
                    last.target,
                    &last.args,
                );
            }
        }
    }
    commands
}

fn goto(
    commands: &mut Vec<Command>,
    ctx: &Context,
    data: &TSlice<FunctionId, FunctionData>,
    functions: &TSlice<FunctionId, Function>,
    f: FunctionId,
    target: Option<BlockId>,
    args: &TSlice<InputId, Option<Arg>>,
) {
    if let Some(target) = target {
        let inputs = functions[f].blocks[target].inputs.iter().copied();
        terminator_memory(commands, data, f, inputs, args);
        commands.push(Command::Move(
            Readable::Block(data[f].blocks[target]),
            Writeable::C,
        ));
        commands.push(Command::Return(
            Readable::C,
            Readable::Section(data[f].blocks[target]),
        ));
    } else {
        terminator_memory(commands, data, f, (0..args.len()).map(LocationId), args);

        commands.push(Command::Move(
            Readable::Block(ctx.stack),
            Writeable::BlockAddr,
        ));
        commands.push(Command::Move(Readable::Byte(0), Writeable::SectionAddr));
        commands.push(Command::Move(Readable::Mem, Writeable::A));
        commands.push(Command::Move(Readable::Byte(2), Writeable::B));
        commands.push(Command::Op(Operation::Sub, Writeable::Mem));
        commands.push(Command::Move(Readable::Byte(1), Writeable::B));
        commands.push(Command::Move(Readable::A, Writeable::SectionAddr));
        commands.push(Command::Move(Readable::Mem, Writeable::C));
        commands.push(Command::Op(Operation::Sub, Writeable::SectionAddr));
        commands.push(Command::Return(Readable::Mem, Readable::C));
    }
}

fn terminator_memory<I>(
    commands: &mut Vec<Command>,
    data: &TSlice<FunctionId, FunctionData>,
    f: FunctionId,
    targets: I,
    args: &TSlice<InputId, Option<Arg>>,
) where
    I: Iterator<Item = LocationId>,
{
    let mut args: TVec<_, _> = args.to_owned();
    let mut used = TBitSet::new();
    for (id, target) in args.index_iter().zip(targets) {
        match args[id] {
            None => (),
            Some(Arg::Location(v)) => {
                used.add(target);
                if v != target {
                    if args[id + 1..].contains(&Some(Arg::Location(target))) {
                        // target location is still needed, move
                        // to an available space
                        let mut free = LocationId(0);
                        while used.get(free) || args[id + 1..].contains(&Some(Arg::Location(free)))
                        {
                            free = free + 1;
                        }

                        for arg in args[id + 1..]
                            .iter_mut()
                            .filter(|&&mut arg| arg == Some(Arg::Location(target)))
                        {
                            *arg = Some(Arg::Location(free));
                        }

                        commands.push(Command::Move(
                            Readable::Block(data[f].storage[target]),
                            Writeable::BlockAddr,
                        ));
                        commands.push(Command::Move(
                            Readable::Section(data[f].storage[target]),
                            Writeable::SectionAddr,
                        ));
                        commands.push(Command::Move(Readable::Mem, Writeable::C));
                        commands.push(Command::Move(
                            Readable::Block(data[f].storage[free]),
                            Writeable::BlockAddr,
                        ));
                        commands.push(Command::Move(
                            Readable::Section(data[f].storage[free]),
                            Writeable::SectionAddr,
                        ));
                        commands.push(Command::Move(Readable::C, Writeable::Mem));
                    }

                    commands.push(Command::Move(
                        Readable::Block(data[f].storage[v]),
                        Writeable::BlockAddr,
                    ));
                    commands.push(Command::Move(
                        Readable::Section(data[f].storage[v]),
                        Writeable::SectionAddr,
                    ));
                    commands.push(Command::Move(Readable::Mem, Writeable::C));
                    commands.push(Command::Move(
                        Readable::Block(data[f].storage[target]),
                        Writeable::BlockAddr,
                    ));
                    commands.push(Command::Move(
                        Readable::Section(data[f].storage[target]),
                        Writeable::SectionAddr,
                    ));
                    commands.push(Command::Move(Readable::C, Writeable::Mem));

                    for arg in args[id + 1..]
                        .iter_mut()
                        .filter(|&&mut arg| arg == Some(Arg::Location(v)))
                    {
                        *arg = Some(Arg::Location(target));
                    }
                }
            }
            Some(Arg::Byte(v)) => {
                used.add(target);
                if args[id + 1..].contains(&Some(Arg::Location(target))) {
                    // target location is still needed, move
                    // to an available space
                    let mut free = LocationId(0);
                    while used.get(free) || args[id + 1..].contains(&Some(Arg::Location(free))) {
                        free = free + 1;
                    }

                    for arg in args[id + 1..]
                        .iter_mut()
                        .filter(|&&mut arg| arg == Some(Arg::Location(target)))
                    {
                        *arg = Some(Arg::Location(free));
                    }

                    commands.push(Command::Move(
                        Readable::Block(data[f].storage[target]),
                        Writeable::BlockAddr,
                    ));
                    commands.push(Command::Move(
                        Readable::Section(data[f].storage[target]),
                        Writeable::SectionAddr,
                    ));
                    commands.push(Command::Move(Readable::Mem, Writeable::C));
                    commands.push(Command::Move(
                        Readable::Block(data[f].storage[free]),
                        Writeable::BlockAddr,
                    ));
                    commands.push(Command::Move(
                        Readable::Section(data[f].storage[free]),
                        Writeable::SectionAddr,
                    ));
                    commands.push(Command::Move(Readable::C, Writeable::Mem));
                }

                commands.push(Command::Move(
                    Readable::Block(data[f].storage[target]),
                    Writeable::BlockAddr,
                ));
                commands.push(Command::Move(
                    Readable::Section(data[f].storage[target]),
                    Writeable::SectionAddr,
                ));
                commands.push(Command::Move(Readable::Byte(v), Writeable::C));
                commands.push(Command::Move(Readable::C, Writeable::Mem));
            }
        }
    }
}
