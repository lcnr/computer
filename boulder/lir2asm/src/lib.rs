use shared_id::{FunctionId, LocationId};

use lir::{Action, Lir};

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
}

impl Readable {
    pub fn is_mem_access(self) -> bool {
        match self {
            Readable::Mem => true,
            _ => false,
        }
    }
}

/// Assembler instructions, we are not using `rock::Command`
/// as there are some commands we only decode later on.
pub enum Command {
    /// moves M1 and M2 so `mem` points to the given location.
    MemStorage(FunctionId, LocationId),
    Move(Readable, Writeable),
    Invert(Writeable),
}

/// converts `lir` to humanly readable assembler.
pub fn convert(_lir: Lir) -> String {
    todo!()
}

pub fn convert_block(lir: &lir::Block, func_id: FunctionId) -> Vec<Command> {
    let mut commands = Vec::new();

    for step in lir.steps.iter() {
        match *step {
            Action::Invert(i, o) => {
                commands.push(Command::MemStorage(func_id, i));
                commands.push(Command::Move(Readable::Mem, Writeable::A));
                commands.push(Command::Invert(Writeable::B));
                commands.push(Command::MemStorage(func_id, o));
                commands.push(Command::Move(Readable::B, Writeable::Mem));
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
            _ => unimplemented!(),
        }
    }
    commands
}
