use crate::token::{Token, TokenType};
use crate::{Cause, Error, ErrorLevel, Logger};

#[derive(Debug, Clone)]
pub enum MemAddr<'a> {
    Named(&'a str),
    Byte(u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Writeable {
    A = 0,
    B = 1,
    C = 2,
    D = 3,
    Mem = 4,
    SectionAddr = 5,
    BlockAddr = 6,
}

impl Writeable {
    pub fn is_mem_access(self) -> bool {
        self == Writeable::Mem
    }
}

#[derive(Debug, Clone)]
pub enum Readable<'a> {
    A,
    B,
    C,
    D,
    Mem,
    MemAddr(MemAddr<'a>),
}
impl Readable<'_> {
    pub fn is_mem_access(&self) -> bool {
        match self {
            Readable::Mem | Readable::MemAddr(_) => true,
            _ => false,
        }
    }

    pub fn command_offset(&self) -> u8 {
        match self {
            Readable::A => 0,
            Readable::B => 1,
            Readable::C => 2,
            Readable::D => 3,
            Readable::Mem => 4,
            Readable::MemAddr(_) => 5,
        }
    }

    fn size(&self) -> u8 {
        if let Readable::MemAddr(_) = self {
            1
        } else {
            0
        }
    }
}

#[derive(Debug, Clone)]
pub enum Condition {
    Zero = 0,
    NotZero = 1,
    GreaterThan = 2,
    GreaterThanOrEqual = 3,
    Equal = 4,
    NotEqual = 5,
    LessThanOrEqual = 6,
    LessThan = 7,
}

#[derive(Debug, Clone)]
pub enum Command<'a> {
    Invalid,
    Section(&'a str),
    Byte(u8),
    Idle,
    Add(Writeable),
    Sub(Writeable),
    And(Writeable),
    Or(Writeable),
    Xor(Writeable),
    Inv(Writeable),
    Shr(Writeable),
    Shl(Writeable),
    Mov(Readable<'a>, Writeable),
    Swap,
    Jmp(Readable<'a>),
    Ljmp(Readable<'a>),
    Ret(Readable<'a>, Readable<'a>),
    If(Condition, Box<Command<'a>>),
}

pub fn parse_commands<'a>(cmd: &Token<'a>, args: &[Token<'a>], l: &mut impl Logger) -> Command<'a> {
    match cmd.origin() {
        "byte" => with_byte(cmd, args, l, Command::Byte),
        "idle" => without_args(cmd, args, l, Command::Idle),
        "add" => with_writeable(cmd, args, l, Command::Add),
        "sub" => with_writeable(cmd, args, l, Command::Sub),
        "and" => with_writeable(cmd, args, l, Command::And),
        "or" => with_writeable(cmd, args, l, Command::Or),
        "xor" => with_writeable(cmd, args, l, Command::Xor),
        "inv" => with_writeable(cmd, args, l, Command::Inv),
        "shr" => with_writeable(cmd, args, l, Command::Shr),
        "shl" => with_writeable(cmd, args, l, Command::Shl),
        "mov" => with_readable_writeable(cmd, args, l, Command::Mov),
        "swap" => without_args(cmd, args, l, Command::Swap),
        "jmp" => with_readable(cmd, args, l, Command::Jmp),
        "ljmp" => with_readable(cmd, args, l, Command::Ljmp),
        "ret" => with_readable_readable(cmd, args, l, Command::Ret),
        "if" => parse_if(cmd, args, l),
        unknown => {
            l.log_err(Error::at_token(
                ErrorLevel::Error,
                Cause::UnknownCommand(unknown),
                cmd,
            ));
            Command::Invalid
        }
    }
}

impl<'a> Command<'a> {
    pub fn new(curr: &mut Vec<Token<'a>>, l: &mut impl Logger) -> Self {
        if curr.len() < 2 {
            l.log_err(Error::expected(
                vec![TokenType::Ident],
                curr.last().unwrap(),
            ));
            return Command::Invalid;
        } else {
            curr.pop();
        }
        let command = curr.first().unwrap();
        if !command.is_ident() {
            l.log_err(Error::expected(vec![TokenType::Ident], command));
            return Command::Invalid;
        }

        parse_commands(command, &curr[1..], l)
    }

    pub fn section(curr: &mut Vec<Token<'a>>, l: &mut impl Logger) -> Self {
        if curr.len() == 2 {
            Command::Section(curr.first().unwrap().origin())
        } else {
            l.log_err(Error::at_token(
                ErrorLevel::Error,
                Cause::InvalidSection,
                curr.first().unwrap(),
            ));
            Command::Invalid
        }
    }

    pub fn size(&self) -> u8 {
        match self {
            Command::Invalid | Command::Section(_) => 0,
            Command::Byte(_)
            | Command::Idle
            | Command::Add(_)
            | Command::Sub(_)
            | Command::And(_)
            | Command::Or(_)
            | Command::Xor(_)
            | Command::Inv(_)
            | Command::Shr(_)
            | Command::Shl(_)
            | Command::Swap => 1,
            Command::Mov(r, _) | Command::Jmp(r) | Command::Ljmp(r) => 1 + r.size(),
            Command::Ret(r, s) => 1 + r.size() + s.size(),
            Command::If(_, cmd) => 1 + cmd.size(),
        }
    }
}

fn as_writeable<'a>(arg: &Token<'a>, l: &mut impl Logger) -> Option<Writeable> {
    Some(match arg.ty() {
        TokenType::A => Writeable::A,
        TokenType::B => Writeable::B,
        TokenType::C => Writeable::C,
        TokenType::D => Writeable::D,
        TokenType::Mem => Writeable::Mem,
        TokenType::SectionAddr => Writeable::SectionAddr,
        TokenType::BlockAddr => Writeable::BlockAddr,
        _ => {
            l.log_err(Error::expected(
                vec![
                    TokenType::A,
                    TokenType::B,
                    TokenType::C,
                    TokenType::D,
                    TokenType::Mem,
                    TokenType::SectionAddr,
                    TokenType::BlockAddr,
                ],
                arg,
            ));
            return None;
        }
    })
}

fn as_readable<'a>(arg: &Token<'a>, l: &mut impl Logger) -> Option<Readable<'a>> {
    Some(match arg.ty() {
        TokenType::A => Readable::A,
        TokenType::B => Readable::B,
        TokenType::C => Readable::C,
        TokenType::D => Readable::D,
        TokenType::Mem => Readable::Mem,
        TokenType::Byte(v) => Readable::MemAddr(MemAddr::Byte(v)),
        TokenType::Ident | TokenType::Section => Readable::MemAddr(MemAddr::Named(arg.origin())),
        _ => {
            l.log_err(Error::expected(
                vec![
                    TokenType::A,
                    TokenType::B,
                    TokenType::C,
                    TokenType::D,
                    TokenType::Mem,
                    TokenType::Byte(0),
                    TokenType::Ident,
                    TokenType::Section,
                ],
                arg,
            ));
            return None;
        }
    })
}

fn parse_if<'a>(cmd: &Token<'a>, args: &[Token<'a>], l: &mut impl Logger) -> Command<'a> {
    if let Some((first, (second, rest))) = args
        .split_first()
        .and_then(|(f, s)| Some((f, s.split_first()?)))
    {
        let condition = match first.origin() {
            "z" => Condition::Zero,
            "nz" => Condition::NotZero,
            "gt" => Condition::GreaterThan,
            "gte" => Condition::GreaterThanOrEqual,
            "eq" => Condition::Equal,
            "neq" => Condition::NotEqual,
            "lte" => Condition::LessThanOrEqual,
            "lt" => Condition::LessThan,
            v => {
                l.log_err(Error::at_token(
                    ErrorLevel::Error,
                    Cause::InvalidCondition(v),
                    first,
                ));
                return Command::Invalid;
            }
        };

        Command::If(condition, Box::new(parse_commands(second, rest, l)))
    } else {
        l.log_err(Error::at_token(
            ErrorLevel::Error,
            Cause::WrongArgCount(cmd.origin(), 3, args.len()),
            cmd,
        ));
        Command::Invalid
    }
}

fn with_readable_readable<'a, F>(
    cmd: &Token<'a>,
    args: &[Token<'a>],
    l: &mut impl Logger,
    f: F,
) -> Command<'a>
where
    F: FnOnce(Readable<'a>, Readable<'a>) -> Command<'a>,
{
    if expect_arg_count(cmd, args, l, 2) {
        if let Some(one) = as_readable(&args[0], l) {
            if let Some(two) = as_readable(&args[1], l) {
                match (&one, &two) {
                    (Readable::MemAddr(_), Readable::MemAddr(_)) => l.log_err(Error::at_token(
                        ErrorLevel::Error,
                        Cause::NonUniqueMemoryAccess(args[0].origin(), args[1].origin()),
                        cmd,
                    )),
                    _ => return f(one, two),
                }
            }
        }
    }

    Command::Invalid
}

fn with_readable_writeable<'a, F>(
    cmd: &Token<'a>,
    args: &[Token<'a>],
    l: &mut impl Logger,
    f: F,
) -> Command<'a>
where
    F: FnOnce(Readable<'a>, Writeable) -> Command<'a>,
{
    if expect_arg_count(cmd, args, l, 2) {
        if let Some(read) = as_readable(&args[0], l) {
            if let Some(write) = as_writeable(&args[1], l) {
                if !(read.is_mem_access() && write.is_mem_access()) {
                    return f(read, write);
                } else {
                    l.log_err(Error::at_token(
                        ErrorLevel::Error,
                        Cause::NonUniqueMemoryAccess(args[0].origin(), args[1].origin()),
                        cmd,
                    ));
                }
            }
        }
    }

    Command::Invalid
}

fn with_readable<'a, F>(
    cmd: &Token<'a>,
    args: &[Token<'a>],
    l: &mut impl Logger,
    f: F,
) -> Command<'a>
where
    F: FnOnce(Readable<'a>) -> Command<'a>,
{
    if expect_arg_count(cmd, args, l, 1) {
        if let Some(read) = as_readable(&args[0], l) {
            f(read)
        } else {
            Command::Invalid
        }
    } else {
        Command::Invalid
    }
}

fn with_writeable<'a, F>(
    cmd: &Token<'a>,
    args: &[Token<'a>],
    l: &mut impl Logger,
    f: F,
) -> Command<'a>
where
    F: FnOnce(Writeable) -> Command<'a>,
{
    if expect_arg_count(cmd, args, l, 1) {
        if let Some(write) = as_writeable(&args[0], l) {
            f(write)
        } else {
            Command::Invalid
        }
    } else {
        Command::Invalid
    }
}

fn with_byte<'a, F>(cmd: &Token<'a>, args: &[Token<'a>], l: &mut impl Logger, f: F) -> Command<'a>
where
    F: FnOnce(u8) -> Command<'a>,
{
    if expect_arg_count(cmd, args, l, 1) {
        if args[0].is_byte() {
            f(args[0].byte_value())
        } else {
            l.log_err(Error::expected(vec![TokenType::Byte(0)], &args[0]));
            Command::Invalid
        }
    } else {
        Command::Invalid
    }
}

fn without_args<'a>(
    cmd: &Token<'a>,
    args: &[Token<'a>],
    l: &mut impl Logger,
    command: Command<'a>,
) -> Command<'a> {
    if args.len() != 0 {
        l.log_err(Error::at_token(
            ErrorLevel::Error,
            Cause::WrongArgCount(cmd.origin(), 0, args.len()),
            &args[0],
        ));
        Command::Invalid
    } else {
        command
    }
}

fn expect_arg_count(
    cmd: &Token<'_>,
    args: &[Token<'_>],
    l: &mut impl Logger,
    expected: usize,
) -> bool {
    if args.len() != expected {
        l.log_err(Error::at_token(
            ErrorLevel::Error,
            Cause::WrongArgCount(cmd.origin(), expected, args.len()),
            cmd,
        ));
        false
    } else {
        true
    }
}
