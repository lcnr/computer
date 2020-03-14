#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use std::{cmp, convert::identity, fmt, iter};

use tindex::{TBitSet, TSlice, TVec};

use shared_id::{BlockId, FunctionId, InputId, LocationId, TagId};

use lir::{Action, Arg, Binop, Function, Lir, Terminator};

mod ctx;
mod optimize;

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
    /// used for batching
    fn from_id(id: usize) -> Self {
        match id {
            0 => Writeable::A,
            1 => Writeable::B,
            2 => Writeable::C,
            _ => unreachable!(),
        }
    }
}

#[allow(unused)]
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
    /// used for batching
    fn from_id(id: usize) -> Self {
        match id {
            0 => Readable::A,
            1 => Readable::B,
            2 => Readable::C,
            _ => unreachable!(),
        }
    }
}

impl Readable {
    fn size(self) -> usize {
        match self {
            Readable::Byte(_) | Readable::Block(_) | Readable::Section(_) => 1,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Void {}

/// A condition of a `Command::If`, used to remove the need for a box.
#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
enum Command<T: fmt::Debug + Clone + CommandSize = Cond> {
    Comment(Box<str>),
    // places a byte into the assembly.
    Byte(u8),
    /// A block internal tag which will be changed into a section address during compilation.
    Tag(TagId),
    Move(Readable, Writeable),
    Op(Operation, Writeable),
    If(T),
    Return(Readable, Readable),
    /// Stop the execution of the given program.
    Halt,
    /// Builds a stack of expected values in the A register. (emulator only)
    Expect(u8),
    /// Checks the top value of the expected stack. (emulator only)
    Check,
    /// Prints the current value of `A`. (emulator only)
    Debug,
}

trait CommandSize {
    fn max_size(&self) -> usize;
}

impl<T: CommandSize + fmt::Debug + Clone> CommandSize for Command<T> {
    fn max_size(&self) -> usize {
        match self {
            Command::Comment(_) | Command::Tag(_) => 0,
            Command::Move(r, _) => 1 + r.size(),
            Command::Op(_, _) | Command::Byte(_) | Command::Halt => 1,
            Command::If(v) => 1 + v.max_size(),
            Command::Return(b, s) => 1 + b.size() + s.size(),
            Command::Expect(_) => 2,
            Command::Check | Command::Debug => 1,
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

impl CommandSize for [Command] {
    fn max_size(&self) -> usize {
        self.iter().map(CommandSize::max_size).sum()
    }
}

impl CommandSize for AsmBlock {
    fn max_size(&self) -> usize {
        self.commands.iter().map(CommandSize::max_size).sum()
    }
}

/// converts `lir` to humanly readable assembler.
pub fn convert(lir: &Lir) -> String {
    #[cfg(feature = "profiler")]
    profile_scope!("convert");

    let mut ctx = Context::new();

    let data: TVec<FunctionId, FunctionData> = lir
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

    let mut asm_blocks: Vec<AsmBlock> = Vec::new();

    for f in lir.functions.index_iter() {
        #[cfg(feature = "profiler")]
        profile_scope!("convert_function");

        if lir.functions[f].ctx.export {
            asm_blocks.push(AsmBlock {
                name: lir.functions[f].name.into(),
                commands: vec![
                    Command::Move(Readable::Block(data[f].blocks[BlockId(0)]), Writeable::C),
                    Command::Return(Readable::C, Readable::Section(data[f].blocks[BlockId(0)])),
                ],
            });
        }

        for b in lir.functions[f].blocks.index_iter() {
            let block_comment =
                Command::Comment(format!("{}[{}]: {}", lir.functions[f].name, f, b).into());
            let block = convert_block(&mut ctx, &data, lir, f, b);

            let block_tag = data[f].blocks[b];
            let block_size = block.max_size();
            if block_size < 256 {
                if let Some(position) = asm_blocks
                    .iter()
                    .position(|asm| 256 - asm.max_size() >= block.max_size())
                {
                    asm_blocks[position]
                        .commands
                        .extend_from_slice(&[block_comment, Command::Tag(block_tag)]);
                    asm_blocks[position].commands.extend(block);
                } else {
                    let mut v = vec![block_comment, Command::Tag(block_tag)];
                    v.extend(block);
                    asm_blocks.push(AsmBlock {
                        name: format!("__block{}", asm_blocks.len()).into(),
                        commands: v,
                    });
                }
            } else {
                let block_section_size = 256
                    - [
                        Command::Move(Readable::Block(TagId(0)), Writeable::D),
                        Command::Return(Readable::D, Readable::Section(TagId(0))),
                    ]
                    .max_size();

                let max_free = asm_blocks
                    .iter()
                    .map(|asm| 256 - asm.max_size())
                    .max()
                    .unwrap_or(0);
                let mut block: &[Command] = &block;
                let mut block_comment = Some(block_comment);
                let mut tag = block_tag;
                while block.max_size() > max_free {
                    let mut v = if let Some(block_comment) = block_comment.take() {
                        vec![block_comment, Command::Tag(tag)]
                    } else {
                        vec![Command::Tag(tag)]
                    };
                    tag = ctx.tm.next();

                    'inner: while let Some((first, rest)) = block.split_first() {
                        if v.max_size() + first.max_size() < block_section_size {
                            block = rest;
                            v.push(first.clone());
                        } else {
                            v.push(Command::Move(Readable::Block(tag), Writeable::D));
                            v.push(Command::Return(Readable::D, Readable::Section(tag)));
                            break 'inner;
                        }
                    }

                    asm_blocks.push(AsmBlock {
                        name: format!("__block{}", asm_blocks.len()).into(),
                        commands: v,
                    });
                }

                let asm_block = &mut asm_blocks
                    .iter_mut()
                    .find(|asm| 256 - asm.max_size() >= block.max_size())
                    .unwrap()
                    .commands;
                asm_block.push(Command::Tag(tag));
                asm_block.extend_from_slice(block);
            }
        }
    }

    for (f, function) in data.index_iter().zip(data.iter()) {
        assert!(function.storage.len() <= 256, "ok");

        let asm_block = if let Some(asm_block) = asm_blocks
            .iter_mut()
            .find(|b| b.max_size() + function.storage.len() <= 256)
        {
            asm_block
        } else {
            asm_blocks.push(AsmBlock {
                name: format!("__block{}", asm_blocks.len()).into(),
                commands: Vec::new(),
            });

            asm_blocks.last_mut().unwrap()
        };

        for (l, &tag) in function.storage.index_iter().zip(function.storage.iter()) {
            asm_block.commands.push(Command::Comment(
                format!("{}[{}]: {}", lir.functions[f].name, f, l).into(),
            ));
            asm_block.commands.push(Command::Tag(tag));
            asm_block.commands.push(Command::Byte(0));
        }
    }

    asm_blocks.push(AsmBlock {
        name: Box::from("stack"),
        commands: vec![Command::Tag(ctx.stack), Command::Byte(0)],
    });

    if lir.functions.iter().any(|f| f.ctx.test) {
        let mut v = Vec::new();

        for (i, (f, function)) in lir
            .functions
            .index_iter()
            .zip(lir.functions.iter())
            .filter(|(_, f)| f.ctx.test)
            .enumerate()
        {
            // put return address on the stack
            let return_adr = ctx.tm.next();
            let func = data[f].storage[LocationId(0)];

            v.extend_from_slice(&[
                Command::Comment(format!("test '{}[{}]'", function.name, f).into()),
                Command::Expect(i as u8),
                Command::Comment(Box::from("prepare return stack")),
                Command::Move(Readable::Block(ctx.stack), Writeable::BlockAddr), // store ret address
                Command::Move(Readable::Byte(0), Writeable::SectionAddr),
                Command::Move(Readable::Mem, Writeable::A),
                Command::Move(Readable::Byte(1), Writeable::B),
                Command::Op(Operation::Add, Writeable::A), // store return block
                Command::Move(Readable::A, Writeable::SectionAddr),
                Command::Move(Readable::Block(return_adr), Writeable::C),
                Command::Move(Readable::C, Writeable::Mem),
                Command::Op(Operation::Add, Writeable::A), // store return section
                Command::Move(Readable::A, Writeable::SectionAddr),
                Command::Move(Readable::Section(return_adr), Writeable::C),
                Command::Move(Readable::C, Writeable::Mem),
                Command::Move(Readable::Byte(0), Writeable::SectionAddr), // update stack ptr
                Command::Move(Readable::A, Writeable::Mem),
                Command::Comment(Box::from("call test function")),
                Command::Move(Readable::Block(data[f].blocks[BlockId(0)]), Writeable::C),
                Command::Return(Readable::C, Readable::Section(data[f].blocks[BlockId(0)])),
                Command::Tag(return_adr),
                Command::Comment(Box::from("check function result")),
                Command::Expect(lir.ctx.true_replacement),
                Command::Move(Readable::Block(func), Writeable::BlockAddr),
                Command::Move(Readable::Section(func), Writeable::SectionAddr),
                Command::Move(Readable::Mem, Writeable::A),
                Command::Check,
                Command::Move(Readable::Byte(i as u8), Writeable::A),
                Command::Check,
            ]);
        }

        v.push(Command::Halt);

        asm_blocks.insert(
            0,
            AsmBlock {
                name: Box::from("__test_runner"),
                commands: v,
            },
        );
    }

    asm_blocks_to_asm(optimize::optimize(asm_blocks))
}

fn convert_block(
    ctx: &mut Context,
    data: &TSlice<FunctionId, FunctionData>,
    lir: &Lir,
    f: FunctionId,
    b: BlockId,
) -> Vec<Command> {
    #[cfg(feature = "profiler")]
    profile_scope!("convert_block");
    let mut commands = Vec::new();

    let block = &lir.functions[f].blocks[b];
    /*
    for l in (0..block.memory_len).map(LocationId) {
        commands.extend_from_slice(&[
            Command::Comment(Box::from(format!("dbg {}", l))),
            Command::Move(Readable::Block(data[f].storage[l]), Writeable::BlockAddr),
            Command::Move(
                Readable::Section(data[f].storage[l]),
                Writeable::SectionAddr,
            ),
            Command::Move(Readable::Mem, Writeable::A),
            Command::Debug,
        ]);
    }
    */

    for s in block.steps.index_iter() {
        let step = &block.steps[s];
        commands.push(Command::Comment(Box::from(format!("{} := {}", s, step))));
        match *step {
            Action::Invert(i, o) => {
                commands.extend_from_slice(&[
                    Command::Move(Readable::Block(data[f].storage[i]), Writeable::BlockAddr),
                    Command::Move(
                        Readable::Section(data[f].storage[i]),
                        Writeable::SectionAddr,
                    ),
                    Command::Move(Readable::Mem, Writeable::A),
                    Command::Move(Readable::Block(data[f].storage[o]), Writeable::BlockAddr),
                    Command::Move(
                        Readable::Section(data[f].storage[o]),
                        Writeable::SectionAddr,
                    ),
                    Command::Op(Operation::Invert, Writeable::Mem),
                ]);
            }
            Action::Move(i, o) => {
                commands.extend_from_slice(&[
                    Command::Move(Readable::Block(data[f].storage[i]), Writeable::BlockAddr),
                    Command::Move(
                        Readable::Section(data[f].storage[i]),
                        Writeable::SectionAddr,
                    ),
                    Command::Move(Readable::Mem, Writeable::A),
                    Command::Move(Readable::Block(data[f].storage[o]), Writeable::BlockAddr),
                    Command::Move(
                        Readable::Section(data[f].storage[o]),
                        Writeable::SectionAddr,
                    ),
                    Command::Move(Readable::A, Writeable::Mem),
                ]);
            }
            Action::Debug(i) => {
                commands.extend_from_slice(&[
                    Command::Move(Readable::Block(data[f].storage[i]), Writeable::BlockAddr),
                    Command::Move(
                        Readable::Section(data[f].storage[i]),
                        Writeable::SectionAddr,
                    ),
                    Command::Move(Readable::Mem, Writeable::A),
                    Command::Debug,
                ]);
            }
            Action::LoadConstant(v, o) => {
                commands.extend_from_slice(&[
                    Command::Move(Readable::Byte(v), Writeable::A),
                    Command::Move(Readable::Block(data[f].storage[o]), Writeable::BlockAddr),
                    Command::Move(
                        Readable::Section(data[f].storage[o]),
                        Writeable::SectionAddr,
                    ),
                    Command::Move(Readable::A, Writeable::Mem),
                ]);
            }
            Action::Binop { op, l, r, out } => {
                match l {
                    Arg::Byte(v) => commands.push(Command::Move(Readable::Byte(v), Writeable::A)),
                    Arg::Location(id) => {
                        commands.extend_from_slice(&[
                            Command::Move(
                                Readable::Block(data[f].storage[id]),
                                Writeable::BlockAddr,
                            ),
                            Command::Move(
                                Readable::Section(data[f].storage[id]),
                                Writeable::SectionAddr,
                            ),
                            Command::Move(Readable::Mem, Writeable::A),
                        ]);
                    }
                }

                match r {
                    Arg::Byte(v) => commands.push(Command::Move(Readable::Byte(v), Writeable::B)),
                    Arg::Location(id) => {
                        commands.extend_from_slice(&[
                            Command::Move(
                                Readable::Block(data[f].storage[id]),
                                Writeable::BlockAddr,
                            ),
                            Command::Move(
                                Readable::Section(data[f].storage[id]),
                                Writeable::SectionAddr,
                            ),
                            Command::Move(Readable::Mem, Writeable::B),
                        ]);
                    }
                }

                commands.extend_from_slice(&[
                    Command::Move(Readable::Block(data[f].storage[out]), Writeable::BlockAddr),
                    Command::Move(
                        Readable::Section(data[f].storage[out]),
                        Writeable::SectionAddr,
                    ),
                ]);
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

                        commands.extend_from_slice(&[
                            Command::Move(Readable::Byte(lir.ctx.false_replacement), Writeable::C),
                            Command::If(cond(
                                op,
                                Command::Move(
                                    Readable::Byte(lir.ctx.true_replacement),
                                    Writeable::C,
                                ),
                            )),
                            Command::Move(Readable::C, Writeable::Mem),
                        ]);
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
                commands.extend_from_slice(&[
                    Command::Move(Readable::Block(ctx.stack), Writeable::BlockAddr),
                    Command::Move(Readable::Byte(0), Writeable::SectionAddr),
                    Command::Move(Readable::Mem, Writeable::A),
                    Command::Move(Readable::Byte(1), Writeable::B),
                ]);

                // put temporaries on the stack
                if !temporaries.is_empty() {
                    for t in temporaries.iter() {
                        commands.extend_from_slice(&[
                            Command::Comment(format!("store {} on stack", t).into()),
                            // load location
                            Command::Move(
                                Readable::Block(data[f].storage[t]),
                                Writeable::BlockAddr,
                            ),
                            Command::Move(
                                Readable::Section(data[f].storage[t]),
                                Writeable::SectionAddr,
                            ),
                            Command::Move(Readable::Mem, Writeable::C),
                            // increment stack head
                            Command::Op(Operation::Add, Writeable::A),
                            Command::Move(Readable::A, Writeable::SectionAddr),
                            Command::Move(Readable::Block(ctx.stack), Writeable::BlockAddr),
                            // save location
                            Command::Move(Readable::C, Writeable::Mem),
                        ]);
                    }
                }

                // put return address on the stack
                let return_adr = ctx.tm.next();

                commands.extend_from_slice(&[
                    Command::Comment(Box::from("store return address on stack")),
                    Command::Op(Operation::Add, Writeable::A),
                    Command::Move(Readable::A, Writeable::SectionAddr),
                    Command::Move(Readable::Block(return_adr), Writeable::C),
                    Command::Move(Readable::C, Writeable::Mem),
                    Command::Op(Operation::Add, Writeable::A),
                    Command::Move(Readable::A, Writeable::SectionAddr),
                    Command::Move(Readable::Section(return_adr), Writeable::C),
                    Command::Move(Readable::C, Writeable::Mem),
                    // update stack ptr
                    Command::Move(Readable::Byte(0), Writeable::SectionAddr),
                    Command::Move(Readable::A, Writeable::Mem),
                ]);
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
                    let mut batch = 0;
                    let mut batch_start = InputId(0);
                    for i in args.index_iter() {
                        if batch == 0 {
                            batch_start = i;
                        }

                        let arg = args[i];
                        match arg {
                            None => {}
                            Some(Arg::Byte(v)) => {
                                commands.push(Command::Move(
                                    Readable::Byte(v),
                                    Writeable::from_id(batch),
                                ));
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
                                commands
                                    .push(Command::Move(Readable::Mem, Writeable::from_id(batch)));
                            }
                        }

                        batch += 1;
                        if batch == 3 {
                            for i in 0..3 {
                                let l = data[id].storage[inputs[batch_start + i]];
                                commands.extend_from_slice(&[
                                    Command::Move(Readable::Block(l), Writeable::BlockAddr),
                                    Command::Move(Readable::Section(l), Writeable::SectionAddr),
                                    Command::Move(Readable::from_id(i), Writeable::Mem),
                                ]);
                            }

                            batch = 0;
                        }
                    }

                    for i in 0..batch {
                        let location = data[id].storage[inputs[batch_start + i]];
                        commands.extend_from_slice(&[
                            Command::Move(Readable::Block(location), Writeable::BlockAddr),
                            Command::Move(Readable::Section(location), Writeable::SectionAddr),
                            Command::Move(Readable::from_id(i), Writeable::Mem),
                        ]);
                    }

                    commands.extend_from_slice(&[
                        Command::Move(Readable::Block(data[id].blocks[BlockId(0)]), Writeable::C),
                        Command::Return(
                            Readable::C,
                            Readable::Section(data[id].blocks[BlockId(0)]),
                        ),
                    ]);
                }

                commands.push(Command::Tag(return_adr));

                // load return arguments
                // TODO: functions currently store their return values at
                // the start of the memory storage, this could be improved
                if id == f {
                    let args: TVec<_, _> = (0..)
                        .map(LocationId)
                        .zip(ret.iter())
                        .filter_map(|(i, ret)| {
                            if ret.is_some() {
                                Some(Some(Arg::Location(i)))
                            } else {
                                None
                            }
                        })
                        .collect();
                    terminator_memory(
                        &mut commands,
                        data,
                        f,
                        ret.iter().copied().filter_map(identity),
                        &args,
                    );
                } else {
                    for (i, &ret) in ret.iter().enumerate() {
                        if let Some(ret) = ret {
                            let i = LocationId(i);

                            commands.extend_from_slice(&[
                                Command::Move(
                                    Readable::Block(data[id].storage[i]),
                                    Writeable::BlockAddr,
                                ),
                                Command::Move(
                                    Readable::Section(data[id].storage[i]),
                                    Writeable::SectionAddr,
                                ),
                                Command::Move(Readable::Mem, Writeable::A),
                                Command::Move(
                                    Readable::Block(data[f].storage[ret]),
                                    Writeable::BlockAddr,
                                ),
                                Command::Move(
                                    Readable::Section(data[f].storage[ret]),
                                    Writeable::SectionAddr,
                                ),
                                Command::Move(Readable::A, Writeable::Mem),
                            ]);
                        }
                    }
                }

                // load temporaries
                // the return address was already popped
                // put temporaries on the stack
                if !temporaries.is_empty() {
                    commands.extend_from_slice(&[
                        Command::Move(Readable::Block(ctx.stack), Writeable::BlockAddr),
                        Command::Move(Readable::Byte(0), Writeable::SectionAddr),
                        Command::Move(Readable::Mem, Writeable::A),
                        Command::Move(Readable::Byte(1), Writeable::B),
                    ]);

                    // TODO: consider batching temporaries
                    for t in temporaries.iter().rev() {
                        commands.extend_from_slice(&[
                            // load temp
                            Command::Move(Readable::Block(ctx.stack), Writeable::BlockAddr),
                            Command::Move(Readable::A, Writeable::SectionAddr),
                            Command::Move(Readable::Mem, Writeable::C),
                            // store temp in location
                            Command::Move(
                                Readable::Block(data[f].storage[t]),
                                Writeable::BlockAddr,
                            ),
                            Command::Move(
                                Readable::Section(data[f].storage[t]),
                                Writeable::SectionAddr,
                            ),
                            Command::Move(Readable::C, Writeable::Mem),
                            // decrement stack ptr
                            Command::Op(Operation::Sub, Writeable::A),
                        ]);
                    }

                    // update stack ptr
                    commands.extend_from_slice(&[
                        Command::Move(Readable::Block(ctx.stack), Writeable::BlockAddr),
                        Command::Move(Readable::Byte(0), Writeable::SectionAddr),
                        Command::Move(Readable::A, Writeable::Mem),
                    ]);
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
    #[cfg(feature = "profiler")]
    profile_scope!("goto");

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

        commands.extend_from_slice(&[
            Command::Move(Readable::Block(ctx.stack), Writeable::BlockAddr),
            Command::Move(Readable::Byte(0), Writeable::SectionAddr),
            Command::Move(Readable::Mem, Writeable::A),
            Command::Move(Readable::Byte(2), Writeable::B),
            Command::Op(Operation::Sub, Writeable::Mem),
            Command::Move(Readable::Byte(1), Writeable::B),
            Command::Move(Readable::A, Writeable::SectionAddr),
            Command::Move(Readable::Mem, Writeable::C),
            Command::Op(Operation::Sub, Writeable::SectionAddr),
            Command::Return(Readable::Mem, Readable::C),
        ]);
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
            None => {}
            Some(Arg::Location(v)) => {
                used.add(target);
                if v != target {
                    // store v in B
                    commands.extend_from_slice(&[
                        Command::Move(Readable::Block(data[f].storage[v]), Writeable::BlockAddr),
                        Command::Move(
                            Readable::Section(data[f].storage[v]),
                            Writeable::SectionAddr,
                        ),
                        Command::Move(Readable::Mem, Writeable::B),
                    ]);

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

                        let free_tag = data[f].storage[free];
                        let target_tag = data[f].storage[target];

                        commands.extend_from_slice(&[
                            Command::Move(Readable::Block(target_tag), Writeable::BlockAddr),
                            Command::Move(Readable::Section(target_tag), Writeable::SectionAddr),
                            Command::Move(Readable::Mem, Writeable::C),
                            Command::Move(Readable::Block(free_tag), Writeable::BlockAddr),
                            Command::Move(Readable::Section(free_tag), Writeable::SectionAddr),
                            Command::Move(Readable::C, Writeable::Mem),
                            Command::Move(Readable::Block(target_tag), Writeable::BlockAddr),
                            Command::Move(Readable::Section(target_tag), Writeable::SectionAddr),
                            Command::Move(Readable::B, Writeable::Mem),
                        ]);

                        if v != free {
                            for arg in args[id + 1..]
                                .iter_mut()
                                .filter(|&&mut arg| arg == Some(Arg::Location(v)))
                            {
                                *arg = Some(Arg::Location(target));
                            }
                        }
                    } else {
                        commands.push(Command::Move(
                            Readable::Block(data[f].storage[target]),
                            Writeable::BlockAddr,
                        ));
                        commands.push(Command::Move(
                            Readable::Section(data[f].storage[target]),
                            Writeable::SectionAddr,
                        ));
                        commands.push(Command::Move(Readable::B, Writeable::Mem));

                        for arg in args[id + 1..]
                            .iter_mut()
                            .filter(|&&mut arg| arg == Some(Arg::Location(v)))
                        {
                            *arg = Some(Arg::Location(target));
                        }
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

                    commands.extend_from_slice(&[
                        Command::Move(
                            Readable::Block(data[f].storage[target]),
                            Writeable::BlockAddr,
                        ),
                        Command::Move(
                            Readable::Section(data[f].storage[target]),
                            Writeable::SectionAddr,
                        ),
                        Command::Move(Readable::Mem, Writeable::C),
                        Command::Move(Readable::Block(data[f].storage[free]), Writeable::BlockAddr),
                        Command::Move(
                            Readable::Section(data[f].storage[free]),
                            Writeable::SectionAddr,
                        ),
                        Command::Move(Readable::C, Writeable::Mem),
                    ]);
                }

                commands.extend_from_slice(&[
                    Command::Move(
                        Readable::Block(data[f].storage[target]),
                        Writeable::BlockAddr,
                    ),
                    Command::Move(
                        Readable::Section(data[f].storage[target]),
                        Writeable::SectionAddr,
                    ),
                    Command::Move(Readable::Byte(v), Writeable::C),
                    Command::Move(Readable::C, Writeable::Mem),
                ]);
            }
        }
    }
}

impl Readable {
    fn to_asm(self, asm_blocks: &[AsmBlock]) -> String {
        match self {
            Readable::A => String::from("A"),
            Readable::B => String::from("B"),
            Readable::C => String::from("C"),
            Readable::D => String::from("D"),
            Readable::Mem => String::from("mem"),
            Readable::Byte(v) => format!("{}", v),
            Readable::Block(s) => {
                for block in asm_blocks.iter() {
                    if block.commands.contains(&Command::Tag(s)) {
                        return format!("{}", block.name);
                    }
                }

                unreachable!("the block tag {} does not exist", s.0);
            }
            Readable::Section(s) => {
                for block in asm_blocks.iter() {
                    if block.commands.contains(&Command::Tag(s)) {
                        return format!("{}.__tag{}", block.name, s.0);
                    }
                }

                unreachable!("the section tag {} does not exist", s.0);
            }
        }
    }
}

#[derive(Debug, Clone)]
struct AsmBlock {
    name: Box<str>,
    commands: Vec<Command>,
}

impl Writeable {
    fn to_asm(self) -> &'static str {
        match self {
            Writeable::A => "A",
            Writeable::B => "B",
            Writeable::C => "C",
            Writeable::D => "D",
            Writeable::Mem => "mem",
            Writeable::BlockAddr => "M2",
            Writeable::SectionAddr => "M1",
        }
    }
}

impl Operation {
    fn to_asm(self) -> &'static str {
        match self {
            Operation::Invert => "inv",
            Operation::Add => "add",
            Operation::Sub => "sub",
            Operation::Shl => "shl",
            Operation::Shr => "shr",
            Operation::BitOr => "or",
            Operation::BitAnd => "and",
            Operation::BitXor => "xor",
        }
    }
}

trait ToAsm {
    fn to_asm(&self, asm_blocks: &[AsmBlock]) -> String;
}

impl ToAsm for Void {
    fn to_asm(&self, _: &[AsmBlock]) -> String {
        unreachable!()
    }
}

impl ToAsm for Cond {
    fn to_asm(&self, asm_blocks: &[AsmBlock]) -> String {
        match self {
            Cond::Zero(cmd) => format!("z {}", cmd.to_asm(asm_blocks)),
            Cond::NZero(cmd) => format!("nz {}", cmd.to_asm(asm_blocks)),
            Cond::Gt(cmd) => format!("gt {}", cmd.to_asm(asm_blocks)),
            Cond::Gte(cmd) => format!("gte {}", cmd.to_asm(asm_blocks)),
            Cond::Eq(cmd) => format!("eq {}", cmd.to_asm(asm_blocks)),
            Cond::Neq(cmd) => format!("neq {}", cmd.to_asm(asm_blocks)),
        }
    }
}

impl<T: Clone + fmt::Debug + CommandSize + ToAsm> Command<T> {
    fn to_asm(&self, asm_blocks: &[AsmBlock]) -> String {
        match self {
            Command::Comment(c) => format!("# {}", c),
            Command::Byte(v) => format!("byte {};", v),
            Command::Tag(id) => format!(".__tag{}:", id.0),
            Command::Move(r, w) => format!("mov {} {};", r.to_asm(&asm_blocks), w.to_asm()),
            Command::Op(op, w) => format!("{} {};", op.to_asm(), w.to_asm()),
            Command::If(cmd) => format!("if {}", cmd.to_asm(asm_blocks)),
            Command::Return(b, s) => {
                format!("ret {} {};", b.to_asm(asm_blocks), s.to_asm(asm_blocks))
            }
            Command::Halt => String::from("halt;"),
            Command::Expect(v) => format!("expect {};", v),
            Command::Check => String::from("check;"),
            Command::Debug => String::from("dbg;"),
        }
    }
}

fn asm_blocks_to_asm(asm_blocks: Vec<AsmBlock>) -> String {
    #[cfg(feature = "profiler")]
    profile_scope!("asm_blocks_to_asm");

    use std::fmt::Write;

    macro_rules! wln {
        ($dst:expr) => { writeln!($dst).unwrap() };
        ($dst:expr,) => { writeln!($dst).unwrap() };
        ($dst:expr, $($arg:tt)*) => { writeln!($dst, $($arg)*).unwrap() };
    }

    let mut s = String::new();
    for block in asm_blocks.iter() {
        wln!(s, "{}:", block.name);
        for (i, cmd) in block.commands.iter().enumerate() {
            if let Command::Tag(_) | Command::Comment(_) = cmd {
                wln!(s, "  {:62} # {:3}", cmd.to_asm(&asm_blocks), i);
            } else {
                wln!(s, "    {:60} # {:3}", cmd.to_asm(&asm_blocks), i);
            }
        }
    }
    s
}
