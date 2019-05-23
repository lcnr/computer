use crate::token::{Token, TokenType};
use crate::{Cause, Error, ErrorLevel, Logger};

#[derive(Debug, Clone)]
pub enum MemAddr<'a> {
    Named(&'a str),
    Byte(u8),
}

#[derive(Debug, Clone)]
pub enum Command<'a> {
    Invalid,
    Section(&'a str),
    Byte(u8),
    Idle,
    Addc(u8),
    Addm,
    Subc(u8),
    Subm,
    Shlc(u8),
    Shlm,
    Shrc(u8),
    Shrm,
    Andc(u8),
    Andm,
    Orc(u8),
    Orm,
    Xorc(u8),
    Xorm,
    Inv,
    Loadc(u8),
    Loadm,
    Store,
    Zero,
    Setsc(u8),
    Setsa,
    Setbc(u8),
    Setba,
    Gets,
    Getb,
    Jmpc(MemAddr<'a>),
    Jmpm,
    Jmpa,
    Ljmpc(MemAddr<'a>),
    Ljmpm,
    Ljmpa,
    Jmpzc(MemAddr<'a>),
    Jmpnzc(MemAddr<'a>),
    Jmpzm,
    Jmpnzm,
    Ljmpzc(MemAddr<'a>),
    Ljmpnzc(MemAddr<'a>),
    Ljmpzm,
    Ljmpnzm,
    Jmpgtcc(u8, MemAddr<'a>),
    Jmpltecc(u8, MemAddr<'a>),
    Jmpltcc(u8, MemAddr<'a>),
    Jmpgtecc(u8, MemAddr<'a>),
    Jmpeqcc(u8, MemAddr<'a>),
    Jmpneqcc(u8, MemAddr<'a>),
    Jmpgtcm(u8),
    Jmpltecm(u8),
    Jmpltcm(u8),
    Jmpgtecm(u8),
    Jmpeqcm(u8),
    Jmpneqcm(u8),
    Jmpgtmc(MemAddr<'a>),
    Jmpltemc(MemAddr<'a>),
    Jmpltmc(MemAddr<'a>),
    Jmpgtemc(MemAddr<'a>),
    Jmpeqmc(MemAddr<'a>),
    Jmpneqmc(MemAddr<'a>),
    Ljmpgtcc(u8, MemAddr<'a>),
    Ljmpltecc(u8, MemAddr<'a>),
    Ljmpltcc(u8, MemAddr<'a>),
    Ljmpgtecc(u8, MemAddr<'a>),
    Ljmpeqcc(u8, MemAddr<'a>),
    Ljmpneqcc(u8, MemAddr<'a>),
    Ljmpgtcm(u8),
    Ljmpltecm(u8),
    Ljmpltcm(u8),
    Ljmpgtecm(u8),
    Ljmpeqcm(u8),
    Ljmpneqcm(u8),
    Ljmpgtmc(MemAddr<'a>),
    Ljmpltemc(MemAddr<'a>),
    Ljmpltmc(MemAddr<'a>),
    Ljmpgtemc(MemAddr<'a>),
    Ljmpeqmc(MemAddr<'a>),
    Ljmpneqmc(MemAddr<'a>),
    Reset,
}

pub fn parse_commands<'a>(cmd: &Token<'a>, args: &[Token<'a>], l: &mut impl Logger) -> Command<'a> {
    match cmd.origin() {
        "byte" => with_byte(cmd, args, l, Command::Byte),
        "idle" => without_args(cmd, args, l, Command::Idle),
        "addc" => with_byte(cmd, args, l, Command::Addc),
        "addm" => without_args(cmd, args, l, Command::Addm),
        "subc" => with_byte(cmd, args, l, Command::Subc),
        "subm" => without_args(cmd, args, l, Command::Subm),
        "shlc" => with_byte(cmd, args, l, Command::Shlc),
        "shlm" => without_args(cmd, args, l, Command::Shlm),
        "shrc" => with_byte(cmd, args, l, Command::Shrc),
        "shrm" => without_args(cmd, args, l, Command::Shrm),
        "andc" => with_byte(cmd, args, l, Command::Andc),
        "andm" => without_args(cmd, args, l, Command::Andm),
        "orc" => with_byte(cmd, args, l, Command::Orc),
        "orm" => without_args(cmd, args, l, Command::Orm),
        "xorc" => with_byte(cmd, args, l, Command::Xorc),
        "xorm" => without_args(cmd, args, l, Command::Xorm),
        "inv" => without_args(cmd, args, l, Command::Inv),
        "loadc" => with_byte(cmd, args, l, Command::Loadc),
        "loadm" => without_args(cmd, args, l, Command::Loadm),
        "store" => without_args(cmd, args, l, Command::Store),
        "zero" => without_args(cmd, args, l, Command::Zero),
        "setsc" => with_byte(cmd, args, l, Command::Setsc),
        "setsa" => without_args(cmd, args, l, Command::Setsa),
        "setbc" => with_byte(cmd, args, l, Command::Setbc),
        "setba" => without_args(cmd, args, l, Command::Setba),
        "gets" => without_args(cmd, args, l, Command::Gets),
        "getb" => without_args(cmd, args, l, Command::Getb),
        "jmpc" => with_section_addr(cmd, args, l, Command::Jmpc),
        "jmpm" => without_args(cmd, args, l, Command::Jmpm),
        "jmpa" => without_args(cmd, args, l, Command::Jmpa),
        "ljmpc" => with_block_addr(cmd, args, l, Command::Ljmpc),
        "ljmpm" => without_args(cmd, args, l, Command::Ljmpm),
        "ljmpa" => without_args(cmd, args, l, Command::Ljmpa),
        "jmpzc" => with_section_addr(cmd, args, l, Command::Jmpzc),
        "jmpnzc" => with_section_addr(cmd, args, l, Command::Jmpnzc),
        "jmpzm" => without_args(cmd, args, l, Command::Jmpzm),
        "jmpnzm" => without_args(cmd, args, l, Command::Jmpnzm),
        "ljmpzc" => with_block_addr(cmd, args, l, Command::Ljmpzc),
        "ljmpnzc" => with_block_addr(cmd, args, l, Command::Ljmpnzc),
        "ljmpzm" => without_args(cmd, args, l, Command::Ljmpzm),
        "ljmpnzm" => without_args(cmd, args, l, Command::Ljmpnzm),
        "jmpgtcc" => with_byte_and_section_addr(cmd, args, l, Command::Jmpgtcc),
        "jmpltecc" => with_byte_and_section_addr(cmd, args, l, Command::Jmpltecc),
        "jmpltcc" => with_byte_and_section_addr(cmd, args, l, Command::Jmpltcc),
        "jmpgtecc" => with_byte_and_section_addr(cmd, args, l, Command::Jmpgtecc),
        "jmpeqcc" => with_byte_and_section_addr(cmd, args, l, Command::Jmpeqcc),
        "jmpneqcc" => with_byte_and_section_addr(cmd, args, l, Command::Jmpneqcc),
        "jmpgtcm" => with_byte(cmd, args, l, Command::Jmpgtcm),
        "jmpltecm" => with_byte(cmd, args, l, Command::Jmpltecm),
        "jmpltcm" => with_byte(cmd, args, l, Command::Jmpltcm),
        "jmpgtecm" => with_byte(cmd, args, l, Command::Jmpgtecm),
        "jmpeqcm" => with_byte(cmd, args, l, Command::Jmpeqcm),
        "jmpneqcm" => with_byte(cmd, args, l, Command::Jmpneqcm),
        "jmpgtmc" => with_section_addr(cmd, args, l, Command::Jmpgtmc),
        "jmpltemc" => with_section_addr(cmd, args, l, Command::Jmpltemc),
        "jmpltmc" => with_section_addr(cmd, args, l, Command::Jmpltmc),
        "jmpgtemc" => with_section_addr(cmd, args, l, Command::Jmpgtemc),
        "jmpeqmc" => with_section_addr(cmd, args, l, Command::Jmpeqmc),
        "jmpneqmc" => with_section_addr(cmd, args, l, Command::Jmpneqmc),
        "ljmpgtcc" => with_byte_and_block_addr(cmd, args, l, Command::Ljmpgtcc),
        "ljmpltecc" => with_byte_and_block_addr(cmd, args, l, Command::Ljmpltecc),
        "ljmpltcc" => with_byte_and_block_addr(cmd, args, l, Command::Ljmpltcc),
        "ljmpgtecc" => with_byte_and_block_addr(cmd, args, l, Command::Ljmpgtecc),
        "ljmpeqcc" => with_byte_and_block_addr(cmd, args, l, Command::Ljmpeqcc),
        "ljmpneqcc" => with_byte_and_block_addr(cmd, args, l, Command::Ljmpneqcc),
        "ljmpgtcm" => with_byte(cmd, args, l, Command::Ljmpgtcm),
        "ljmpltecm" => with_byte(cmd, args, l, Command::Ljmpltecm),
        "ljmpltcm" => with_byte(cmd, args, l, Command::Ljmpltcm),
        "ljmpgtecm" => with_byte(cmd, args, l, Command::Ljmpgtecm),
        "ljmpeqcm" => with_byte(cmd, args, l, Command::Ljmpeqcm),
        "ljmpneqcm" => with_byte(cmd, args, l, Command::Ljmpneqcm),
        "ljmpgtmc" => with_block_addr(cmd, args, l, Command::Ljmpgtmc),
        "ljmpltemc" => with_block_addr(cmd, args, l, Command::Ljmpltemc),
        "ljmpltmc" => with_block_addr(cmd, args, l, Command::Ljmpltmc),
        "ljmpgtemc" => with_block_addr(cmd, args, l, Command::Ljmpgtemc),
        "ljmpeqmc" => with_block_addr(cmd, args, l, Command::Ljmpeqmc),
        "ljmpneqmc" => with_block_addr(cmd, args, l, Command::Ljmpneqmc),
        "reset" => without_args(cmd, args, l, Command::Reset),
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
            | Command::Inv
            | Command::Addm
            | Command::Subm
            | Command::Shlm
            | Command::Shrm
            | Command::Andm
            | Command::Orm
            | Command::Xorm
            | Command::Loadm
            | Command::Store
            | Command::Zero
            | Command::Setsa
            | Command::Setba
            | Command::Gets
            | Command::Getb
            | Command::Jmpm
            | Command::Jmpa
            | Command::Ljmpm
            | Command::Ljmpa
            | Command::Reset => 1,
            Command::Addc(_)
            | Command::Subc(_)
            | Command::Shlc(_)
            | Command::Shrc(_)
            | Command::Andc(_)
            | Command::Orc(_)
            | Command::Xorc(_)
            | Command::Loadc(_)
            | Command::Setsc(_)
            | Command::Setbc(_)
            | Command::Jmpc(_)
            | Command::Ljmpc(_)
            | Command::Jmpzm
            | Command::Jmpnzm
            | Command::Ljmpzm
            | Command::Ljmpnzm => 2,
            Command::Jmpzc(_)
            | Command::Jmpnzc(_)
            | Command::Ljmpzc(_)
            | Command::Ljmpnzc(_)
            | Command::Jmpgtcm(_)
            | Command::Jmpltecm(_)
            | Command::Jmpltcm(_)
            | Command::Jmpgtecm(_)
            | Command::Jmpeqcm(_)
            | Command::Jmpneqcm(_)
            | Command::Jmpgtmc(_)
            | Command::Jmpltemc(_)
            | Command::Jmpltmc(_)
            | Command::Jmpgtemc(_)
            | Command::Jmpeqmc(_)
            | Command::Jmpneqmc(_)
            | Command::Ljmpgtcm(_)
            | Command::Ljmpltecm(_)
            | Command::Ljmpltcm(_)
            | Command::Ljmpgtecm(_)
            | Command::Ljmpeqcm(_)
            | Command::Ljmpneqcm(_)
            | Command::Ljmpgtmc(_)
            | Command::Ljmpltemc(_)
            | Command::Ljmpltmc(_)
            | Command::Ljmpgtemc(_)
            | Command::Ljmpeqmc(_)
            | Command::Ljmpneqmc(_) => 3,
            Command::Jmpgtcc(_, _)
            | Command::Jmpltecc(_, _)
            | Command::Jmpltcc(_, _)
            | Command::Jmpgtecc(_, _)
            | Command::Jmpeqcc(_, _)
            | Command::Jmpneqcc(_, _)
            | Command::Ljmpgtcc(_, _)
            | Command::Ljmpltecc(_, _)
            | Command::Ljmpltcc(_, _)
            | Command::Ljmpgtecc(_, _)
            | Command::Ljmpeqcc(_, _)
            | Command::Ljmpneqcc(_, _) => 4,
        }
    }
}

fn with_byte_and_block_addr<'a, F>(
    cmd: &Token<'a>,
    args: &[Token<'a>],
    l: &mut impl Logger,
    f: F,
) -> Command<'a>
where
    F: FnOnce(u8, MemAddr<'a>) -> Command<'a>,
{
    if expect_arg_count(cmd, args, l, 2) {
        if args[0].is_byte() {
            let value = args[0].byte_value();
            if args[1].is_byte() {
                f(value, MemAddr::Byte(args[1].byte_value()))
            } else if args[1].is_ident() {
                f(value, MemAddr::Named(args[1].origin()))
            } else {
                l.log_err(Error::expected(
                    vec![TokenType::Byte(0), TokenType::Ident],
                    &args[1],
                ));
                Command::Invalid
            }
        } else {
            l.log_err(Error::expected(vec![TokenType::Byte(0)], &args[0]));
            Command::Invalid
        }
    } else {
        l.log_err(Error::at_token(
            ErrorLevel::Error,
            Cause::WrongArgCount(cmd.origin(), 2, args.len()),
            &args[0],
        ));
        Command::Invalid
    }
}

fn with_block_addr<'a, F>(
    cmd: &Token<'a>,
    args: &[Token<'a>],
    l: &mut impl Logger,
    f: F,
) -> Command<'a>
where
    F: FnOnce(MemAddr<'a>) -> Command<'a>,
{
    if expect_arg_count(cmd, args, l, 1) {
        if args[0].is_byte() {
            f(MemAddr::Byte(args[0].byte_value()))
        } else if args[0].is_ident() {
            f(MemAddr::Named(args[0].origin()))
        } else {
            l.log_err(Error::expected(
                vec![TokenType::Byte(0), TokenType::Ident],
                &args[0],
            ));
            Command::Invalid
        }
    } else {
        l.log_err(Error::at_token(
            ErrorLevel::Error,
            Cause::WrongArgCount(cmd.origin(), 1, args.len()),
            &args[0],
        ));
        Command::Invalid
    }
}

fn with_byte_and_section_addr<'a, F>(
    cmd: &Token<'a>,
    args: &[Token<'a>],
    l: &mut impl Logger,
    f: F,
) -> Command<'a>
where
    F: FnOnce(u8, MemAddr<'a>) -> Command<'a>,
{
    if expect_arg_count(cmd, args, l, 2) {
        if args[0].is_byte() {
            let value = args[0].byte_value();
            if args[1].is_byte() {
                f(value, MemAddr::Byte(args[1].byte_value()))
            } else if args[1].is_section() || args[1].is_ident() {
                f(value, MemAddr::Named(args[1].origin()))
            } else {
                l.log_err(Error::expected(
                    vec![TokenType::Byte(0), TokenType::Ident, TokenType::Section],
                    &args[1],
                ));
                Command::Invalid
            }
        } else {
            l.log_err(Error::expected(vec![TokenType::Byte(0)], &args[0]));
            Command::Invalid
        }
    } else {
        l.log_err(Error::at_token(
            ErrorLevel::Error,
            Cause::WrongArgCount(cmd.origin(), 2, args.len()),
            &args[0],
        ));
        Command::Invalid
    }
}

fn with_section_addr<'a, F>(
    cmd: &Token<'a>,
    args: &[Token<'a>],
    l: &mut impl Logger,
    f: F,
) -> Command<'a>
where
    F: FnOnce(MemAddr<'a>) -> Command<'a>,
{
    if expect_arg_count(cmd, args, l, 1) {
        if args[0].is_byte() {
            f(MemAddr::Byte(args[0].byte_value()))
        } else if args[0].is_section() || args[0].is_ident() {
            f(MemAddr::Named(args[0].origin()))
        } else {
            l.log_err(Error::expected(
                vec![TokenType::Byte(0), TokenType::Ident, TokenType::Section],
                &args[0],
            ));
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
            Cause::WrongArgCount(cmd.origin(), 0, args.len()),
            cmd,
        ));
        false
    } else {
        true
    }
}
