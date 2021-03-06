use std::fmt::{Display, Formatter, Result};

use tindex::TIndex;

use shared_id::{FunctionId, StepId, TypeId};

use crate::{binop::Binop, Action, MatchArm, Mir, Object, Terminator, Type, UnaryOperation};

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Invert => write!(f, "invert"),
            Self::ToBytes => write!(f, "to_bytes"),
            Self::FromBytes => write!(f, "from_bytes"),
            Self::Debug => write!(f, "dbg"),
            Self::BlackBox => write!(f, "black_box"),
        }
    }
}

impl Display for Binop {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::Mul => write!(f, "mul"),
            Self::Div => write!(f, "div"),
            Self::Rem => write!(f, "rem"),
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

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Uninhabited => write!(f, "uninhabited"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::Struct(fields) => {
                write!(f, "struct(")?;
                if let Some((last, start)) = fields.split_last() {
                    for arg in start.iter() {
                        write!(f, "{}, ", arg)?;
                    }
                    write!(f, "{}", last)?;
                }
                write!(f, ")")
            }
            Type::Union(fields) => {
                write!(f, "union(")?;
                let mut iter = fields.iter();
                if let Some(first) = iter.next() {
                    write!(f, "{}", first)?;
                    for arg in iter {
                        write!(f, ", {}", arg)?;
                    }
                }
                write!(f, ")")
            }
            Type::Sum(cases) => {
                write!(f, "sum(")?;
                let mut iter = cases.iter();
                if let Some(first) = iter.next() {
                    write!(f, "{}", first)?;
                    for arg in iter {
                        write!(f, ", {}", arg)?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Object::Unit => write!(f, "Unit"),
            Object::U8(v) => write!(f, "{}u8", v),
            Object::U16(v) => write!(f, "{}u16", v),
            Object::U32(v) => write!(f, "{}u32", v),
            Object::Struct(fields) => {
                write!(f, "struct(")?;
                if let Some((last, start)) = fields.split_last() {
                    for arg in start.iter() {
                        write!(f, "{}, ", arg)?;
                    }
                    write!(f, "{}", last)?;
                }
                write!(f, ")")
            }
            Object::Variant(id, obj) => write!(f, "{}: {}", id, obj),
            Object::Field(id, obj) => write!(f, "union.{}: {}", id.as_index(), obj),
            Object::Undefined => write!(f, "undef"),
        }
    }
}

impl<'a> Display for Mir<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (i, ty) in self.types.iter().enumerate() {
            let id: TypeId = i.into();
            writeln!(f, "type {}: {}", id, ty)?;
        }

        for (i, func) in self.functions.iter().enumerate() {
            let func_id: FunctionId = i.into();
            if func.ctx.is_test {
                writeln!(f, "@test")?;
            }
            if func.ctx.export {
                writeln!(f, "@export")?;
            }
            writeln!(f, "fn {}[{}] -> {}:", func.name, func_id, func.ret)?;

            if func.ctx.hidden {
                writeln!(f, "    [HIDDEN]",)?;
                continue;
            }

            for (i, block) in func.blocks.iter().enumerate() {
                write!(f, "  block ~{}(", i)?;
                if let Some((last, start)) = block.input.split_last() {
                    for (i, arg) in start.iter().enumerate() {
                        write!(f, "!{}: {}, ", i, arg)?;
                    }
                    write!(f, "!{}: {}", start.len(), last)?;
                }
                writeln!(f, "):")?;

                for (i, step) in block.steps.iter().enumerate() {
                    write!(f, "    ${}: {} := ", i, step.ty)?;
                    match &step.action {
                        Action::Extend(id) => writeln!(f, "extend {}", id),
                        Action::Reduce(id) => writeln!(f, "reduce {}", id),
                        Action::LoadConstant(obj) => writeln!(f, "load {}", obj),
                        Action::LoadInput(i) => writeln!(f, "load !{}", i),
                        Action::InitializeStruct(fields) => {
                            write!(f, "init struct(")?;
                            if let Some((last, start)) = fields.split_last() {
                                for arg in start.iter() {
                                    write!(f, "{}, ", arg)?;
                                }
                                write!(f, "{}", last)?;
                            }
                            writeln!(f, ")")
                        }
                        &Action::InitializeUnion(id) => {
                            writeln!(f, "init union({}: {})", block[id].ty, id)
                        }
                        Action::CallFunction(i, args) => {
                            write!(f, "call {}(", i)?;
                            if let Some((last, start)) = args.split_last() {
                                for arg in start.iter() {
                                    write!(f, "{}, ", arg)?;
                                }
                                write!(f, "{}", last)?;
                            }
                            writeln!(f, ")")
                        }
                        Action::StructFieldAccess(s, a) => writeln!(f, "{}.{}", s, a.as_index()),
                        Action::UnionFieldAccess(s) => writeln!(f, "{} as {}", s, step.ty),
                        Action::UnaryOperation(kind, expr) => writeln!(f, "{} {}", kind, expr),
                        Action::Binop(kind, a, b) => writeln!(f, "{} {} {}", kind, a, b),
                    }?;
                }

                write!(f, "    ")?;
                match &block.terminator {
                    Terminator::Goto(block, args) => {
                        if let Some(block) = block {
                            write!(f, "goto {}(", block)?;
                        } else {
                            write!(f, "return(")?;
                        }

                        if let Some((last, start)) = args.split_last() {
                            for arg in start.iter() {
                                write!(f, "{}, ", arg)?;
                            }
                            write!(f, "{}", last)?;
                        }
                        writeln!(f, ")")
                    }
                    &Terminator::Match(id, ref arms) => print_match(f, id, arms),
                    &Terminator::MatchByte(id, ref arms) => print_match(f, id, arms),
                }?;
            }
        }

        Ok(())
    }
}

fn print_match<T: Display>(f: &mut Formatter, id: StepId, arms: &[MatchArm<T>]) -> Result {
    let write_arm = |f: &mut Formatter, arm: &MatchArm<T>| {
        if let Some(block) = arm.target {
            write!(f, "{} -> goto ~{}(", arm.pat, block.0)?;
        } else {
            write!(f, "{} -> return(", arm.pat)?;
        }

        if let Some((last, start)) = arm.args.split_last() {
            for arg in start.iter() {
                if let Some(arg) = arg {
                    write!(f, "${}, ", arg.0)?;
                } else {
                    write!(f, "self, ")?;
                }
            }

            if let Some(arg) = last {
                write!(f, "${}", arg.0)?;
            } else {
                write!(f, "self")?;
            }
        }
        write!(f, ")")
    };

    write!(f, "match ${}(", id.0)?;
    if let Some((last, start)) = arms.split_last() {
        for arm in start.iter() {
            write_arm(f, arm)?;
            write!(f, ", ")?;
        }
        write_arm(f, last)?;
    }
    writeln!(f, ")")
}
