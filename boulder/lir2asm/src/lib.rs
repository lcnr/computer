use std::{convert::TryFrom, fmt::Write};

use tindex::TVec;

use shared_id::{BlockId, FunctionId};

use lir::{Function, Lir};

/// Assembler instructions, we are not using `rock::Command`
pub enum Command {

}

/// converts `lir` to humanly readable assembler.
pub fn convert(lir: Lir) -> String {
    todo!()
}

pub fn convert_block(lir: lir::Block) -> Vec<Command> {
    todo!()
}