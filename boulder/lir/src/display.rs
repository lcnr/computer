use std::fmt::{Display, Formatter, Result};

use shared_id::FunctionId;

use crate::{Action, Binop, Lir};

impl Display for Binop {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::Shl => write!(f, "shl"),
            Self::Shr => write!(f, "shr"),
            Self::Eq => write!(f, "eq"),
            Self::Neq => write!(f, "neq"),
            Self::Gt => write!(f, "gt"),
            Self::Gte => write!(f, "gte"),
            Self::BitOr => write!(f, "bitor"),
            Self::BitAnd => write!(f, "bitand"),
            Self::BitXor => write!(f, "bitxor"),
        }
    }
}

fn write_list<T: Display>(f: &mut Formatter<'_>, args: &[T]) -> Result {
    if let Some((last, start)) = args.split_last() {
        for arg in start.iter() {
            write!(f, "{}, ", arg)?;
        }
        write!(f, "{}", last)
    } else {
        Ok(())
    }
}

impl<'a> Display for Lir<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (i, func) in self.functions.iter().enumerate() {
            let func_id: FunctionId = i.into();
            if func.ctx.test {
                writeln!(f, "@test")?;
            }
            if func.ctx.export {
                writeln!(f, "@export")?;
            }
            writeln!(
                f,
                "fn {}[{}] -> {}:",
                func.name, func_id, func.return_length
            )?;

            if func.ctx.hidden {
                writeln!(f, "    [HIDDEN]",)?;
                continue;
            }

            for (i, block) in func.blocks.iter().enumerate() {
                write!(
                    f,
                    "  block ~{}(input: {}, memory: {})",
                    i, block.input_len, block.memory_len
                )?;

                for (i, step) in block.steps.iter().enumerate() {
                    write!(f, "    ${} := ", i)?;
                    match step {
                        Action::Invert(i, o) => write!(f, "invert {} -> {}", i, o),
                        Action::Debug(i) => write!(f, "debug {}", i),
                        Action::LoadInput(idx, o) => write!(f, "load !{} -> {}", idx, o),
                        Action::LoadConstant(v, o) => write!(f, "load {} -> {}", v, o),
                        Action::Binop { op, l, r, out } => {
                            write!(f, "{} {} {} -> {}", op, l, r, out)
                        }
                        Action::FunctionCall { id, args, ret } => {
                            write!(f, "call {}(", id)?;
                            write_list(f, args)?;
                            write!(f, ") -> (")?;
                            write_list(f, ret)?;
                            write!(f, ")")
                        }
                    }?;
                    writeln!(f)?;
                }

                write!(f, "    TERMINATOR TODO")?;
            }
        }

        Ok(())
    }
}
