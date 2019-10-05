use std::fmt::{Display, Formatter, Result};

use tindex::TIndex;

use shared_id::{FunctionId, TypeId};

use crate::{binop::Binop, Action, BlockId, Mir, Object, StepId, Terminator, Type, UnaryOperation};

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Invert => write!(f, "invert"),
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
            Self::Shl => write!(f, "shl"),
            Self::Shr => write!(f, "shr"),
            Self::Eq => write!(f, "eq"),
            Self::Neq => write!(f, "neq"),
            Self::Gt => write!(f, "gt"),
            Self::Gte => write!(f, "gte"),
            Self::BitOr => write!(f, "bitor"),
            Self::BitAnd => write!(f, "bitand"),
        }
    }
}

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
                        write!(f, "{}, ", arg)?;
                    }
                    write!(f, "{}", last)?;
                }
                write!(f, ")")
            }
            Type::Sum(cases) => {
                write!(f, "sum(")?;
                let mut iter = cases.iter();
                if let Some(first) = iter.next() {
                    write!(f, "{}", first)?;
                    for arg in iter {
                        write!(f, " | {}", arg)?;
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
        }
    }
}

impl Display for Mir {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (i, ty) in self.types.iter().enumerate() {
            let id: TypeId = i.into();
            writeln!(f, "type {}: {}", id, ty)?;
        }

        for (i, func) in self.functions.iter().enumerate() {
            let func_id: FunctionId = i.into();
            writeln!(f, "fn {}[{}]:", func.name, func_id)?;
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
                        Action::Extend(id) => writeln!(f, "extend ${}", id.0),
                        Action::LoadConstant(obj) => writeln!(f, "load {}", obj),
                        Action::LoadInput(i) => writeln!(f, "load !{}", i),
                        Action::InitializeStruct(ty, fields) => {
                            write!(f, "init {}(", ty)?;
                            if let Some((last, start)) = fields.split_last() {
                                for arg in start.iter() {
                                    write!(f, "${}, ", arg.0)?;
                                }
                                write!(f, "${}", last.0)?;
                            }
                            writeln!(f, ")")
                        }
                        Action::CallFunction(i, args) => {
                            write!(f, "call {}(", i)?;
                            if let Some((last, start)) = args.split_last() {
                                for arg in start.iter() {
                                    write!(f, "${}, ", arg.0)?;
                                }
                                write!(f, "${}", last.0)?;
                            }
                            writeln!(f, ")")
                        }

                        Action::FieldAccess(s, a) => writeln!(f, "${}.{}", s.0, a.as_index()),
                        Action::UnaryOperation(kind, expr) => writeln!(f, "{} ${}", kind, expr.0),
                        Action::Binop(kind, a, b) => writeln!(f, "{} ${} ${}", kind, a.0, b.0),
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
                            write!(f, "{} -> ~{}(", ty, block.0)?;
                            if let Some((last, start)) = args.split_last() {
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
                }?;
            }
        }

        Ok(())
    }
}
