mod token;

use std::collections::HashMap;

#[derive(Clone, Copy, Debug)]
pub struct CodeGenError;

#[derive(Debug, Clone)]
pub enum Cause<'a> {
    MissingStartBlock,
    UnknownCommand(&'a str),
    InvalidToken {
        expected: Vec<TokenType>,
        found: TokenType,
    },
    InvalidSection,
    UnspecifiedError,
    WrongArgCount(&'a str, usize, usize),
    /// only 256 blocks can be stored
    BlockCount(usize),
    /// the same block position is used twice
    BlockReuse,
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorLevel {
    Warn,
    Error,
}

#[derive(Debug, Clone)]
pub struct Error<'a> {
    lvl: ErrorLevel,
    cause: Cause<'a>,
    line: usize,
    origin: &'a str,
}

impl<'a> Error<'a> {
    pub fn new(lvl: ErrorLevel, cause: Cause<'a>, line: usize, origin: &'a str) -> Self {
        Self {
            lvl,
            cause,
            line,
            origin,
        }
    }

    pub fn at_token(lvl: ErrorLevel, cause: Cause<'a>, tok: &Token<'a>) -> Self {
        Self {
            lvl,
            cause,
            line: tok.line(),
            origin: tok.origin(),
        }
    }

    /// Error: expected ... found ...
    fn expected(expected: Vec<TokenType>, tok: &Token<'a>) -> Self {
        Self {
            lvl: ErrorLevel::Error,
            cause: Cause::InvalidToken {
                expected,
                found: tok.ty(),
            },
            line: tok.line(),
            origin: tok.origin(),
        }
    }

    pub fn lvl(&self) -> ErrorLevel {
        self.lvl
    }

    pub fn cause(&self) -> &Cause<'a> {
        &self.cause
    }
}

use token::{Token, TokenIter, TokenType};
#[derive(Debug, Clone)]
pub enum Command<'a> {
    Invalid,
    Section(&'a str, usize),
    Idle,
    Inv,
}

impl<'a> Command<'a> {
    fn new(curr: &mut Vec<Token<'a>>, l: &mut impl Logger) -> Self {
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

        match command.origin() {
            "idle" => {
                if curr.len() != 1 {
                    l.log_err(Error::at_token(
                        ErrorLevel::Error,
                        Cause::WrongArgCount(command.origin(), 1, curr.len()),
                        command,
                    ));
                    Command::Invalid
                } else {
                    Command::Idle
                }
            }
            unknown => {
                l.log_err(Error::at_token(
                    ErrorLevel::Error,
                    Cause::UnknownCommand(unknown),
                    command,
                ));
                Command::Invalid
            }
        }
    }

    fn section(curr: &mut Vec<Token<'a>>, l: &mut impl Logger) -> Self {
        if curr.len() == 2 {
            Command::Section(curr.first().unwrap().origin(), 0)
        } else {
            l.log_err(Error::at_token(
                ErrorLevel::Error,
                Cause::InvalidSection,
                curr.first().unwrap(),
            ));
            Command::Invalid
        }
    }

    fn size(&self) -> usize {
        match self {
            Command::Invalid | Command::Section(_, _) => 0,
            Command::Idle | Command::Inv => 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block<'a> {
    name: &'a str,
    pos: Option<u8>,
    line: usize,
    content: Vec<Command<'a>>,
}

impl<'a> Block<'a> {
    fn new<L: Logger>(curr: &mut Vec<Token<'a>>, l: &mut L) -> Self {
        match curr.len() {
            3 => {
                let b = &curr[1];
                let a = &curr[0];
                if a.is_ident() {
                    if b.is_byte() {
                        Block {
                            name: a.origin(),
                            pos: Some(b.byte_value()),
                            line: a.line(),
                            content: Vec::new(),
                        }
                    } else {
                        l.log_err(Error::expected(vec![TokenType::Byte(0)], &b));
                        Block::invalid(b.line())
                    }
                } else {
                    l.log_err(Error::expected(vec![TokenType::Ident], &a));
                    Block::invalid(a.line())
                }
            }
            2 => {
                let a = &curr[0];
                if a.is_ident() {
                    Block {
                        name: a.origin(),
                        pos: None,
                        line: a.line(),
                        content: Vec::new(),
                    }
                } else if a.is_byte() {
                    Block {
                        name: "<unnamed>",
                        pos: Some(a.byte_value()),
                        line: a.line(),
                        content: Vec::new(),
                    }
                } else {
                    l.log_err(Error::expected(vec![TokenType::Ident], a));
                    Block::invalid(a.line())
                }
            }
            _ => {
                let tok = curr.first().unwrap();
                l.log_err(Error::at_token(
                    ErrorLevel::Error,
                    Cause::UnspecifiedError,
                    tok,
                ));

                Block::invalid(tok.line())
            }
        }
    }

    fn invalid(line: usize) -> Self {
        Self {
            name: "<invalid>",
            pos: None,
            line,
            content: Vec::new(),
        }
    }

    fn push(&mut self, command: Command<'a>) {
        self.content.push(command)
    }
}

fn current_block<'a, 'b: 'a>(
    blocks: &'a mut Vec<Block<'b>>,
    src: &'b str,
    l: &mut impl Logger,
) -> &'a mut Block<'b> {
    if blocks.is_empty() {
        l.log_err(Error::new(
            ErrorLevel::Warn,
            Cause::MissingStartBlock,
            1,
            src.split(';').next().unwrap(),
        ));
        let block = Block::invalid(1);
        blocks.push(block);
    }

    blocks.last_mut().unwrap()
}

pub trait Logger {
    fn log_err(&mut self, err: Error<'_>);
}

pub struct DebugLogger;

impl Logger for DebugLogger {
    fn log_err(&mut self, err: Error<'_>) {
        println!("{:?}", err);
    }
}

pub fn parse<'a, L: Logger>(src: &'a str, l: &mut L) -> Result<Vec<Block<'a>>, CodeGenError> {
    let mut blocks = Vec::new();

    let mut curr: Vec<Token> = Vec::with_capacity(4);
    for token in TokenIter::new(src) {
        let ty = token.ty();
        curr.push(token);
        match ty {
            TokenType::Colon => {
                if curr.first().map(|c| c.is_section()).unwrap_or(false) {
                    let current_block = current_block(&mut blocks, src, l);
                    let command = Command::section(&mut curr, l);
                    current_block.push(command);
                } else {
                    blocks.push(Block::new(&mut curr, l));
                }
                curr.clear();
            }
            TokenType::SemiColon => {
                let current_block = current_block(&mut blocks, src, l);
                let command = Command::new(&mut curr, l);
                current_block.push(command);
                curr.clear();
            }
            _ => (),
        }
    }

    if !curr.is_empty() {
        let current_block = current_block(&mut blocks, src, l);
        let command = Command::new(&mut curr, l);
        current_block.push(command);
    }

    if blocks.len() > 256 {
        let problematic = &blocks[256];
        l.log_err(Error::new(
            ErrorLevel::Error,
            Cause::BlockCount(blocks.len()),
            problematic.line,
            problematic.name,
        ));
    }

    resolve(&mut blocks, l)?;
    Ok(blocks)
}

fn register<'a>(blocks: &[Block<'a>]) -> HashMap<&'a str, u8> {
    let mut map = HashMap::new();

    for block in blocks.iter() {
        map.insert(block.name, block.pos.unwrap());
    }

    map
}

pub fn resolve<'a>(blocks: &mut [Block<'a>], l: &mut impl Logger) -> Result<(), CodeGenError> {
    let mut used = [false; 256];
    for block in blocks {
        if let Some(pos) = block.pos {
            if used[pos as usize] == true {
                l.log_err(Error::new(
                    ErrorLevel::Error,
                    Cause::BlockReuse,
                    block.line,
                    block.name,
                ));
                return Err(CodeGenError);
            }
        }
    }

    Ok(())
}
