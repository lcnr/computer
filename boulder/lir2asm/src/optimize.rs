use tindex::TBitSet;

use shared_id::TagId;

use crate::{AsmBlock, Command, Readable, Writeable};

/// This function does not invalidate the following invariants:
///
/// - the first block remains first
pub(crate) fn optimize(mut asm_blocks: Vec<AsmBlock>) -> Vec<AsmBlock> {
    // tags may still be moved between blocks

    // TODO: optimize asm

    // tags may NOT be moved between blocks
    dedup_loads(&mut asm_blocks);

    asm_blocks
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Value {
    Byte(u8),
    /// read from the current memory location
    Memory,
    Section(TagId),
    Block(TagId),
}

enum Read {
    Reg(usize),
    Mem,
    Value(Value),
}

impl Readable {
    fn reg(self) -> Read {
        match self {
            Readable::A => Read::Reg(0),
            Readable::B => Read::Reg(1),
            Readable::C => Read::Reg(2),
            Readable::D => Read::Reg(3),
            Readable::Mem => Read::Mem,
            Readable::Byte(v) => Read::Value(Value::Byte(v)),
            Readable::Block(id) => Read::Value(Value::Block(id)),
            Readable::Section(id) => Read::Value(Value::Section(id)),
        }
    }
}

impl Writeable {
    fn reg(self) -> Option<usize> {
        match self {
            Writeable::A => Some(0),
            Writeable::B => Some(1),
            Writeable::C => Some(2),
            Writeable::D => Some(3),
            Writeable::Mem => None,
            Writeable::SectionAddr => Some(4),
            Writeable::BlockAddr => Some(5),
        }
    }
}

/// TODO: consider storing the memory locations with a known value
fn dedup_loads(blocks: &mut [AsmBlock]) {
    for block in blocks.iter_mut() {
        let mut regs = [None; 6];
        let mut mem = Value::Memory;
        let mut to_remove = TBitSet::new();
        for (c, command) in block.commands.iter_mut().enumerate() {
            match command {
                Command::Comment(_)
                | Command::Byte(_)
                | Command::Expect(_)
                | Command::Check
                | Command::Debug => (),
                // TODO: we currently reset all registers as the tag might be
                // used as a jump address, this is pessimistic and can be improved.
                Command::Tag(_) => {
                    regs = [None; 6];
                    mem = Value::Memory;
                }
                Command::Move(r, w) => match (r.reg(), w.reg()) {
                    (Read::Reg(r), Some(w)) => {
                        if regs[w].is_some() && regs[r] == regs[w] {
                            to_remove.add(c);
                        } else {
                            regs[w] = regs[r];
                            if w == 4 || w == 5 {
                                for v in regs.iter_mut() {
                                    if *v == Some(Value::Memory) {
                                        *v = None;
                                    }
                                }
                                mem = Value::Memory;
                            }
                        }
                    }
                    (Read::Reg(r), None) => {
                        if let Some(v) = regs[r] {
                            mem = v;
                        }

                        if regs[r] != Some(Value::Memory) {
                            for v in regs.iter_mut() {
                                if *v == Some(Value::Memory) {
                                    *v = None;
                                }
                            }
                        }
                    }
                    (Read::Mem, Some(w)) => {
                        if Some(mem) == regs[w] {
                            to_remove.add(c);
                        } else {
                            regs[w] = Some(mem);
                            if w == 4 || w == 5 {
                                for v in regs.iter_mut() {
                                    if *v == Some(Value::Memory) {
                                        *v = None;
                                    }
                                }
                                mem = Value::Memory;
                            }
                        }
                    }
                    (Read::Value(v), Some(w)) => {
                        if Some(v) == regs[w] {
                            to_remove.add(c);
                        } else {
                            regs[w] = Some(v);
                            if w == 4 || w == 5 {
                                for v in regs.iter_mut() {
                                    if *v == Some(Value::Memory) {
                                        *v = None;
                                    }
                                }
                                mem = Value::Memory;
                            }
                        }
                    }
                    (_, None) => unreachable!("move mem mem"),
                },
                Command::Op(_, w) => {
                    // TODO: consider computing the operation.
                    if let Some(w) = w.reg() {
                        regs[w] = None;
                    } else {
                        for v in regs.iter_mut() {
                            if *v == Some(Value::Memory) {
                                *v = None;
                            }
                        }
                        mem = Value::Memory;
                    }
                }
                Command::If(_) => {
                    // TODO: actually look into the if statement.
                    regs = [None; 6];
                    mem = Value::Memory;
                }
                Command::Return(_, _) | Command::Halt => {
                    // TODO: remove all steps between this command and the next tag.
                }
            }
        }

        for i in to_remove.into_iter().rev() {
            block.commands.remove(i);
        }
    }
}
