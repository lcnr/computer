use crate::token::{Token, TokenType};
use crate::{Cause, Error, ErrorLevel, Logger};

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
            | Command::Xorm => 1,
            Command::Addc(_)
            | Command::Subc(_)
            | Command::Shlc(_)
            | Command::Shrc(_)
            | Command::Andc(_)
            | Command::Orc(_)
            | Command::Xorc(_) => 2,
        }
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
        l.log_err(Error::at_token(
            ErrorLevel::Error,
            Cause::WrongArgCount(cmd.origin(), 1, args.len()),
            &args[0],
        ));
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
