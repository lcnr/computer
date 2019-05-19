use std::collections::HashMap;
use std::mem;

mod commands;
mod token;

use commands::{Command, MemAddr};
use token::{Token, TokenIter, TokenType};

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
    InvalidBlock,
    EmptyBlock,
    UnspecifiedError,
    WrongArgCount(&'a str, usize, usize),
    /// only 256 blocks can be stored
    BlockCount(usize),
    /// the same block position is used twice
    BlockReuse,
    /// more than 256 bytes in one block
    BlockSize,
    MissingTerminator,
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

pub fn codegen<L: Logger>(src: &str, logger: &mut L) -> Result<Vec<u8>, CodeGenError> {
    let mut blocks = parse(src, logger)?;
    resolve(&mut blocks, logger)?;

    for block in blocks.iter() {
        if block.name == "<invalid>"
            || block.content.iter().any(|cmd| match cmd {
                Command::Invalid => true,
                _ => false,
            })
        {
            return Err(CodeGenError);
        }
    }

    Ok(finalize(blocks))
}

fn parse<'a, L: Logger>(src: &'a str, l: &mut L) -> Result<Vec<Block<'a>>, CodeGenError> {
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
        l.log_err(Error::at_token(
            ErrorLevel::Error,
            Cause::MissingTerminator,
            curr.last().unwrap(),
        ));
        return Err(CodeGenError);
    }

    if blocks.len() > 256 {
        let problematic = &blocks[256];
        l.log_err(Error::new(
            ErrorLevel::Error,
            Cause::BlockCount(blocks.len()),
            problematic.line,
            problematic.name,
        ));
        return Err(CodeGenError);
    }

    Ok(blocks)
}

fn register_blocks<'a>(blocks: &[Block<'a>]) -> HashMap<&'a str, u8> {
    let mut map = HashMap::new();

    for block in blocks.iter() {
        map.insert(block.name, block.pos.unwrap());
    }

    map
}

fn register_sections<'a>(
    block: &Block<'a>,
    l: &mut impl Logger,
) -> Result<HashMap<&'a str, u8>, CodeGenError> {
    let mut map: HashMap<&str, u8> = HashMap::new();

    let mut addr = 0u8;
    for cmd in block.content.iter() {
        match cmd {
            Command::Section(s) => mem::drop(map.insert(s, addr)),
            c => {
                addr = if let Some(addr) = addr.checked_add(c.size()) {
                    addr
                } else {
                    l.log_err(Error::new(
                        ErrorLevel::Error,
                        Cause::BlockSize,
                        block.line,
                        block.name,
                    ));
                    return Err(CodeGenError);
                }
            }
        }
    }

    if addr == 0 {
        l.log_err(Error::new(
            ErrorLevel::Warn,
            Cause::EmptyBlock,
            block.line,
            block.name,
        ));
    }

    Ok(map)
}

pub fn resolve<'a, L: Logger>(blocks: &mut [Block<'a>], l: &mut L) -> Result<(), CodeGenError> {
    let mut used = [false; 256];
    for block in blocks.iter() {
        if let Some(pos) = block.pos {
            if used[pos as usize] == true {
                l.log_err(Error::new(
                    ErrorLevel::Error,
                    Cause::BlockReuse,
                    block.line,
                    block.name,
                ));
                return Err(CodeGenError);
            } else {
                used[pos as usize] = true;
            }
        }
    }

    let mut idx = 0;
    for block in blocks.iter_mut() {
        if block.pos.is_none() {
            while used[idx] {
                idx += 1;
            }
            used[idx] = true;
            block.pos = Some(idx as u8);
        }
    }

    let ljmp_addr = register_blocks(blocks);
    for block in blocks {
        let jmp_addr = register_sections(&block, l)?;

        let line = block.line;
        let name = block.name;
        let replace_block_addr = |s: &str, l: &mut L| {
            if let Some(&byte) = ljmp_addr.get(s) {
                Ok(MemAddr::Byte(byte))
            } else {
                l.log_err(Error::new(
                    ErrorLevel::Error,
                    Cause::InvalidSection,
                    line,
                    name,
                ));
                Err(CodeGenError)
            }
        };

        let replace_section_addr = |s: &str, l: &mut L| {
            if s.starts_with(".") {
                if let Some(&byte) = jmp_addr.get(s) {
                    Ok(MemAddr::Byte(byte))
                } else {
                    l.log_err(Error::new(
                        ErrorLevel::Error,
                        Cause::InvalidSection,
                        line,
                        name,
                    ));
                    Err(CodeGenError)
                }
            } else if s == name {
                Ok(MemAddr::Byte(0))
            } else {
                l.log_err(Error::new(
                    ErrorLevel::Error,
                    Cause::InvalidBlock,
                    line,
                    name,
                ));
                Err(CodeGenError)
            }
        };

        for cmd in block.content.iter_mut() {
            match cmd {
                Command::Jmpc(ref mut addr) => {
                    if let MemAddr::Named(s) = addr {
                        *addr = replace_section_addr(s, l)?
                    }
                }
                Command::Ljmpc(ref mut addr) => {
                    if let MemAddr::Named(s) = addr {
                        *addr = replace_block_addr(s, l)?
                    }
                }
                Command::Jmpzc(ref mut addr) => {
                    if let MemAddr::Named(s) = addr {
                        *addr = replace_section_addr(s, l)?
                    }
                }
                Command::Jmpnzc(ref mut addr) => {
                    if let MemAddr::Named(s) = addr {
                        *addr = replace_section_addr(s, l)?
                    }
                }
                Command::Ljmpzc(ref mut addr) => {
                    if let MemAddr::Named(s) = addr {
                        *addr = replace_block_addr(s, l)?
                    }
                }
                Command::Ljmpnzc(ref mut addr) => {
                    if let MemAddr::Named(s) = addr {
                        *addr = replace_block_addr(s, l)?
                    }
                }
                _ => (),
            }
        }
    }

    Ok(())
}

fn finalize(mut blocks: Vec<Block<'_>>) -> Vec<u8> {
    if blocks.is_empty() {
        return Vec::new();
    }

    blocks.sort_by_key(|b| b.pos.unwrap());
    let mut res = Vec::with_capacity((blocks.last().unwrap().pos.unwrap() as usize + 1) * 256);
    for block in blocks {
        res.resize(block.pos.unwrap() as usize * 256, 0);
        for cmd in block.content {
            match cmd {
                Command::Byte(v) => res.push(v),
                Command::Idle => res.push(0x00),
                Command::Addc(v) => res.extend_from_slice(&[0x01, v]),
                Command::Addm => res.push(0x02),
                Command::Subc(v) => res.extend_from_slice(&[0x03, v]),
                Command::Subm => res.push(0x04),
                Command::Shlc(v) => res.extend_from_slice(&[0x05, v]),
                Command::Shlm => res.push(0x06),
                Command::Shrc(v) => res.extend_from_slice(&[0x07, v]),
                Command::Shrm => res.push(0x08),
                Command::Andc(v) => res.extend_from_slice(&[0x09, v]),
                Command::Andm => res.push(0x0a),
                Command::Orc(v) => res.extend_from_slice(&[0x0b, v]),
                Command::Orm => res.push(0x0c),
                Command::Xorc(v) => res.extend_from_slice(&[0x0d, v]),
                Command::Xorm => res.push(0x0e),
                Command::Inv => res.push(0x0f),
                Command::Loadc(v) => res.extend_from_slice(&[0x10, v]),
                Command::Loadm => res.push(0x11),
                Command::Store => res.push(0x12),
                Command::Zero => res.push(0x13),
                Command::Setsc(v) => res.extend_from_slice(&[0x14, v]),
                Command::Setsa => res.push(0x15),
                Command::Setbc(v) => res.extend_from_slice(&[0x16, v]),
                Command::Setba => res.push(0x17),
                Command::Gets => res.push(0x18),
                Command::Getb => res.push(0x19),
                Command::Jmpc(MemAddr::Byte(v)) => res.extend_from_slice(&[0x1a, v]),
                Command::Jmpm => res.push(0x1b),
                Command::Jmpa => res.push(0x1c),
                Command::Ljmpc(MemAddr::Byte(v)) => res.extend_from_slice(&[0x1d, v]),
                Command::Ljmpm => res.push(0x1e),
                Command::Ljmpa => res.push(0x1f),
                Command::Jmpzc(MemAddr::Byte(v)) => res.extend_from_slice(&[0x20, 0x21, v]),
                Command::Jmpnzc(MemAddr::Byte(v)) => res.extend_from_slice(&[0x21, 0x20, v]),
                Command::Jmpzm => res.extend_from_slice(&[0x22, 0x23]),
                Command::Jmpnzm => res.extend_from_slice(&[0x23, 0x22]),
                Command::Ljmpzc(MemAddr::Byte(v)) => res.extend_from_slice(&[0x24, 0x25, v]),
                Command::Ljmpnzc(MemAddr::Byte(v)) => res.extend_from_slice(&[0x25, 0x24, v]),
                Command::Ljmpzm => res.extend_from_slice(&[0x26, 0x27]),
                Command::Ljmpnzm => res.extend_from_slice(&[0x27, 0x26]),
                Command::Section(_) => (),
                cmd @ Command::Invalid
                | cmd @ Command::Jmpc(MemAddr::Named(_))
                | cmd @ Command::Ljmpc(MemAddr::Named(_))
                | cmd @ Command::Jmpzc(MemAddr::Named(_))
                | cmd @ Command::Jmpnzc(MemAddr::Named(_))
                | cmd @ Command::Ljmpzc(MemAddr::Named(_))
                | cmd @ Command::Ljmpnzc(MemAddr::Named(_)) => {
                    unreachable!("unexpected command: {:?}", cmd)
                }
            };
        }
    }

    res
}
