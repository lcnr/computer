use tindex::{TBitSet, TSlice};

use shared_id::{BlockId, TagId};

use crate::{AsmBlock, Command, Readable, Writeable};

/// This function does not invalidate the following invariants:
///
/// - the first block remains first
pub(crate) fn optimize(mut asm_blocks: Vec<AsmBlock>) -> Vec<AsmBlock> {
    #[cfg(feature = "profiler")]
    profile_scope!("optimize");

    // tags may still be moved between blocks

    // TODO: optimize asm

    // tags may NOT be moved between blocks
    dedup_loads(<&mut TSlice<_, _>>::from(asm_blocks.as_mut_slice()));

    asm_blocks
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Value {
    Byte(u8),
    /// read from the current memory location
    Memory,
    Section(TagId),
    Block(BlockId),
}

enum Read {
    Reg(usize),
    Mem,
    Value(Value),
}

impl Readable {
    fn reg(self, blocks: &TSlice<BlockId, AsmBlock>) -> Read {
        match self {
            Readable::A => Read::Reg(0),
            Readable::B => Read::Reg(1),
            Readable::C => Read::Reg(2),
            Readable::D => Read::Reg(3),
            Readable::Mem => Read::Mem,
            Readable::Byte(v) => Read::Value(Value::Byte(v)),
            Readable::Block(id) => blocks
                .iter()
                .position(|b| b.commands.contains(&Command::Tag(id)))
                .map(BlockId)
                .map(|p| Read::Value(Value::Block(p)))
                .unwrap(),
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
fn dedup_loads(blocks: &mut TSlice<BlockId, AsmBlock>) {
    #[cfg(feature = "profiler")]
    profile_scope!("dedup_loads");
    fn update_reg(
        regs: &mut [Option<Value>; 6],
        mem: &mut Value,
        w: Option<usize>,
        r: Option<Value>,
    ) {
        if let Some(w) = w {
            regs[w] = r;
            if w == 4 || w == 5 {
                for v in regs.iter_mut() {
                    if *v == Some(Value::Memory) {
                        *v = None;
                    }
                }
                *mem = Value::Memory;
            }
        } else {
            if let Some(v) = r {
                *mem = v;
            } else {
                *mem = Value::Memory;
            }

            if r != Some(Value::Memory) {
                for v in regs.iter_mut() {
                    if *v == Some(Value::Memory) {
                        *v = None;
                    }
                }
            }
        }
    }

    for b in blocks.index_iter() {
        let block = &blocks[b];
        let mut regs = [None; 6];
        let mut mem = Value::Memory;
        let mut to_remove = TBitSet::new();
        for (c, command) in block.commands.iter().enumerate() {
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
                Command::Move(r, w) => match (r.reg(&blocks), w.reg()) {
                    (Read::Reg(r), Some(w)) => {
                        if regs[w].is_some() && regs[r] == regs[w] {
                            to_remove.add(c);
                        } else {
                            let value = regs[r];
                            update_reg(&mut regs, &mut mem, Some(w), value);
                        }
                    }
                    (Read::Reg(r), None) => {
                        let value = regs[r];
                        update_reg(&mut regs, &mut mem, None, value);
                    }
                    (Read::Mem, Some(w)) => {
                        if Some(mem) == regs[w] {
                            to_remove.add(c);
                        } else {
                            let value = Some(mem);
                            update_reg(&mut regs, &mut mem, Some(w), value);
                        }
                    }
                    (Read::Value(v), Some(w)) => {
                        if Some(v) == regs[w] {
                            to_remove.add(c);
                        } else {
                            update_reg(&mut regs, &mut mem, Some(w), Some(v));
                        }
                    }
                    (_, None) => unreachable!("move mem mem"),
                },
                Command::Op(_, w) => {
                    // TODO: consider computing the operation.
                    update_reg(&mut regs, &mut mem, w.reg(), None);
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

        let block = &mut blocks[b];
        for i in to_remove.into_iter().rev() {
            block.commands.remove(i);
        }
    }
}
