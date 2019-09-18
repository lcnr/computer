use crate::*;

use std::fmt::{Display, Formatter, Result};

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Uninhabited => write!(f, "Uninhabited"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::Struct(fields) => {
                write!(f, "struct(")?;
                if let Some((last, start)) = fields.split_last() {
                    for arg in start.iter() {
                        write!(f, "%{}, ", arg.0)?;
                    }
                    write!(f, "%{}", last.0)?;
                }
                write!(f, ")")
            }
            Type::Sum(cases) => {
                write!(f, "sum(")?;
                if let Some((last, start)) = cases.split_last() {
                    for arg in start.iter() {
                        write!(f, "%{} | ", arg.0)?;
                    }
                    write!(f, "%{}", last.0)?;
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
        }
    }
}

impl Display for Mir {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (i, ty) in self.types.iter().enumerate() {
            writeln!(f, "type %{}: {}", i, ty)?;
        }

        for (i, func) in self.functions.iter().enumerate() {
            writeln!(f, "fn {}[#{}]:", func.name, i)?;
            for (i, block) in func.content.iter().enumerate() {
                write!(f, "  block ~{}(", i)?;
                if let Some((last, start)) = block.input.split_last() {
                    for (i, arg) in start.iter().enumerate() {
                        write!(f, "!{}: %{}, ", i, arg.0)?;
                    }
                    write!(f, "!{}: %{}", start.len(), last.0)?;
                }
                writeln!(f, "):")?;

                for (i, step) in block.content.iter().enumerate() {
                    write!(f, "    ${}: %{} := ", i, step.ty.0)?;
                    match &step.action {
                        Action::Extend(id) => writeln!(f, "extend ${}", id.0),
                        Action::LoadConstant(obj) => writeln!(f, "load {}", obj),
                        Action::LoadInput(i) => writeln!(f, "load !{}", i),
                        Action::CallFunction(i, args) => {
                            write!(f, "call #{}(", i.0)?;
                            if let Some((last, start)) = args.split_last() {
                                for arg in start.iter() {
                                    write!(f, "${}, ", arg.0)?;
                                }
                                write!(f, "${}", last.0)?;
                            }
                            writeln!(f, ")")
                        }

                        Action::FieldAccess(s, a) => writeln!(f, "${}.{}", s.0, a.0),
                        Action::Add(a, b) => writeln!(f, "add ${} ${}", a.0, b.0),
                        Action::Sub(a, b) => writeln!(f, "sub ${} ${}", a.0, b.0),
                        Action::Mul(a, b) => writeln!(f, "mul ${} ${}", a.0, b.0),
                        Action::Div(a, b) => writeln!(f, "div ${} ${}", a.0, b.0),
                        Action::Lt(a, b) => writeln!(f, "cmp ${} < ${}", a.0, b.0),
                        Action::BitOr(a, b) => writeln!(f, "bitor ${} $ {}", a.0, b.0),
                    }?;
                }

                write!(f, "    ")?;
                match &block.terminator {
                    Terminator::Return(v) => writeln!(f, "return ${}", v.0),
                    Terminator::Goto(block, args) => {
                        write!(f, "goto ~{}(", block.0)?;
                        if let Some((last, start)) = args.split_last() {
                            for arg in start.iter() {
                                write!(f, "${}, ", arg.0)?;
                            }
                            write!(f, "${}", last.0)?;
                        }
                        writeln!(f, ")")
                    }
                    Terminator::Match(id, arms) => {
                        let write_arm = |f: &mut Formatter,
                                         (ty, block, args): &(
                            TypeId,
                            BlockId,
                            Vec<Option<StepId>>,
                        )| {
                            write!(f, "%{} -> ~{}(", ty.0, block.0)?;
                            if let Some((last, start)) = args.split_last() {
                                for arg in start.iter() {
                                    if let Some(arg) = arg {
                                        write!(f, "${}, ", arg.0)?;
                                    } else {
                                        write!(f, "$self")?;
                                    }
                                }

                                if let Some(arg) = last {
                                    write!(f, "${}, ", arg.0)?;
                                } else {
                                    write!(f, "$self")?;
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
                }?;
            }
        }

        Ok(())
    }
}
