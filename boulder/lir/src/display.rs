use std::fmt::{Display, Formatter, Result};

use shared_id::{FunctionId, LocationId};

use crate::{Action, Arg, Binop, Function, Lir, MatchArm, Terminator};

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

impl Display for Arg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Arg::Byte(v) => write!(f, "b{}", v),
            Arg::Location(id) => id.fmt(f),
        }
    }
}

impl Display for Action {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Action::Invert(i, o) => write!(f, "invert {} -> {}", i, o),
            Action::Move(i, o) => write!(f, "move {} -> {}", i, o),
            Action::Debug(i) => write!(f, "debug {}", i),
            Action::LoadConstant(v, o) => write!(f, "load b{} -> {}", v, o),
            Action::Binop { op, l, r, out } => write!(f, "{} {} {} -> {}", op, l, r, out),
            Action::FunctionCall { id, args, ret } => {
                write!(f, "call {}(", id)?;
                write_maybe_list(f, args)?;
                write!(f, ") -> (")?;
                write_maybe_list(f, ret)?;
                write!(f, ")")
            }
        }
    }
}

impl Display for Terminator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Terminator::Goto(block, args) => {
                if let Some(block) = block {
                    write!(f, "goto {}(", block)?;
                } else {
                    write!(f, "return (")?;
                }

                write_maybe_list(f, args)?;
                write!(f, ")")
            }
            &Terminator::Match(id, ref arms) => print_match(f, id, arms),
        }
    }
}

fn write_list<T: Display, I: IntoIterator<Item = T>>(f: &mut Formatter<'_>, elems: I) -> Result {
    let mut iter = elems.into_iter();
    if let Some(first) = iter.next() {
        write!(f, "{}", first)?;
        for elem in iter {
            write!(f, ", {}", elem)?;
        }
    }
    Ok(())
}

fn write_maybe_list<'a, T: Display + 'a, I: IntoIterator<Item = &'a Option<T>> + 'a>(
    f: &mut Formatter<'_>,
    elems: I,
) -> Result {
    let mut iter = elems.into_iter();
    if let Some(first) = iter.next() {
        if let Some(v) = first {
            write!(f, "{}", v)?;
        } else {
            write!(f, "_")?;
        }
        for elem in iter {
            if let Some(v) = elem {
                write!(f, ", {}", v)?;
            } else {
                write!(f, ", _")?;
            }
        }
    }
    Ok(())
}

pub struct FunctionDisplay<'a, 'b> {
    inner: &'a Function<'b>,
    id: FunctionId,
}

impl<'a, 'b> Display for FunctionDisplay<'a, 'b> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let func = self.inner;

        if func.ctx.test {
            writeln!(f, "@test")?;
        }
        if func.ctx.export {
            writeln!(f, "@export")?;
        }
        writeln!(
            f,
            "fn {}[{}] -> {}:",
            func.name, self.id, func.return_length
        )?;

        if func.ctx.hidden {
            writeln!(f, "    [HIDDEN]",)?;
            return Ok(());
        }

        for (i, block) in func.blocks.iter().enumerate() {
            write!(f, "  block ~{}[memory: {}](", i, block.memory_len)?;
            write_list(f, &block.inputs)?;
            writeln!(f, ")")?;

            for (i, step) in block.steps.iter().enumerate() {
                writeln!(f, "    ${} := {}", i, step)?;
            }

            writeln!(f, "    {}", block.terminator)?;
        }

        Ok(())
    }
}

impl<'a> Function<'a> {
    pub fn display<'b>(&'b self, id: FunctionId) -> FunctionDisplay<'b, 'a> {
        FunctionDisplay { inner: self, id }
    }
}

impl<'a> Display for Lir<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (i, func) in self.functions.iter().enumerate() {
            let id: FunctionId = i.into();
            writeln!(f, "{}", func.display(id))?;
        }

        Ok(())
    }
}

fn print_match(f: &mut Formatter, id: LocationId, arms: &[MatchArm]) -> Result {
    let write_arm = |f: &mut Formatter, arm: &MatchArm| {
        if let Some(block) = arm.target {
            write!(f, "b{} -> goto {}(", arm.pat, block)?;
        } else {
            write!(f, "b{} -> return (", arm.pat)?;
        }

        write_maybe_list(f, &arm.args)?;
        write!(f, ")")
    };

    write!(f, "match {}(", id)?;
    if let Some((last, start)) = arms.split_last() {
        for arm in start.iter() {
            write_arm(f, arm)?;
            write!(f, ", ")?;
        }
        write_arm(f, last)?;
    }
    write!(f, ")")
}
