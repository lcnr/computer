use std::collections::HashMap;

pub mod commands;
mod token;

use commands::{Command, MemAddr, Readable};
use token::{Token, TokenIter, TokenType};

#[derive(Clone, Copy, Debug)]
pub struct CodeGenError;

#[derive(Debug, Clone)]
pub enum Cause<'a> {
    MissingStartBlock,
    UnknownCommand(&'a str),
    InvalidCondition(&'a str),
    InvalidToken {
        expected: Vec<TokenType>,
        found: TokenType,
    },
    RepeatingSectionIdentifier(&'a str),
    NestedIf,
    InvalidSection,
    InvalidBlock,
    EmptyBlock,
    UnspecifiedError,
    NonUniqueMemoryAccess(&'a str, &'a str),
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
    pub fn custom(name: &'a str, pos: Option<u8>, line: usize) -> Self {
        Block {
            name,
            pos,
            line,
            content: Vec::new(),
        }
    }

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

pub fn compile<L: Logger>(src: &str, logger: &mut L) -> Result<Vec<u8>, CodeGenError> {
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
            Command::Section(s) => map.insert(s, addr).map_or(Ok(()), |_| {
                l.log_err(Error::new(
                    ErrorLevel::Error,
                    Cause::RepeatingSectionIdentifier(s),
                    block.line,
                    block.name,
                ));
                Err(CodeGenError)
            })?,
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
            if used[pos as usize] {
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

    let jmp_addr = blocks
        .iter()
        .map(|b| Ok((b.name, register_sections(&b, l)?)))
        .collect::<Result<HashMap<&str, HashMap<&str, u8>>, CodeGenError>>()?;

    for block in blocks.iter_mut() {
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
                    s,
                ));
                Err(CodeGenError)
            }
        };

        let replace_section_addr = |s: &str, l: &mut L| {
            if s.starts_with('.') {
                if let Some(&byte) = jmp_addr.get(name).unwrap().get(s) {
                    Ok(MemAddr::Byte(byte))
                } else {
                    l.log_err(Error::new(
                        ErrorLevel::Error,
                        Cause::InvalidSection,
                        line,
                        s,
                    ));
                    Err(CodeGenError)
                }
            } else if let Some(pos) = s.bytes().position(|b| b == b'.') {
                let block = &s[..pos];
                let section = &s[pos..];
                if let Some(block) = jmp_addr.get(block) {
                    if let Some(&byte) = block.get(section) {
                        Ok(MemAddr::Byte(byte))
                    } else {
                        l.log_err(Error::new(
                            ErrorLevel::Error,
                            Cause::InvalidSection,
                            line,
                            section,
                        ));
                        Err(CodeGenError)
                    }
                } else {
                    l.log_err(Error::new(
                        ErrorLevel::Error,
                        Cause::InvalidBlock,
                        line,
                        block,
                    ));
                    Err(CodeGenError)
                }
            } else if s == name {
                Ok(MemAddr::Byte(0))
            } else {
                l.log_err(Error::new(ErrorLevel::Error, Cause::InvalidBlock, line, s));
                Err(CodeGenError)
            }
        };

        for mut cmd in block.content.iter_mut() {
            if let Command::If(_, cond) = cmd {
                cmd = cond;
            }

            match cmd {
                Command::Mov(Readable::MemAddr(ref mut addr), _)
                | Command::Jmp(Readable::MemAddr(ref mut addr))
                | Command::Ret(_, Readable::MemAddr(ref mut addr)) => {
                    if let MemAddr::Named(s) = addr {
                        *addr = replace_section_addr(s, l)?
                    }
                }
                Command::Ljmp(Readable::MemAddr(ref mut addr))
                | Command::Ret(Readable::MemAddr(ref mut addr), _) => {
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
        for mut cmd in block.content {
            if let Command::If(cond, expr) = cmd {
                res.push(0xc0 + cond as u8 + (expr.size() - 1) * 8);
                cmd = *expr;
            }

            match cmd {
                Command::Byte(v) => res.push(v),
                Command::Idle => res.push(0x00),
                Command::Add(write) => res.push(0x01 + write as u8),
                Command::Sub(write) => res.push(0x08 + write as u8),
                Command::And(write) => res.push(0x0f + write as u8),
                Command::Or(write) => res.push(0x16 + write as u8),
                Command::Xor(write) => res.push(0x1d + write as u8),
                Command::Inv(write) => res.push(0x24 + write as u8),
                Command::Shr(write) => res.push(0x2b + write as u8),
                Command::Shl(write) => res.push(0x32 + write as u8),
                Command::Mov(read, write) => {
                    let (r, mut w) = (read.command_offset(), write as u8);
                    if r <= w {
                        w -= 1;
                    }
                    res.push(0x39 + 6 * r + w);
                    if let Readable::MemAddr(addr) = read {
                        if let MemAddr::Byte(v) = addr {
                            res.push(v);
                        } else {
                            unreachable!("mov with named mem addr");
                        }
                    }
                }
                Command::Swap => res.push(0x5d),
                Command::Jmp(read) => {
                    res.push(0x60 + read.command_offset());
                    if let Readable::MemAddr(addr) = read {
                        if let MemAddr::Byte(v) = addr {
                            res.push(v);
                        } else {
                            unreachable!("jmp with named mem addr");
                        }
                    }
                }
                Command::Ljmp(read) => {
                    res.push(0x66 + read.command_offset());
                    if let Readable::MemAddr(addr) = read {
                        if let MemAddr::Byte(v) = addr {
                            res.push(v);
                        } else {
                            unreachable!("ljmp with named mem addr");
                        }
                    }
                }
                Command::Ret(r, s) => {
                    let (ro, mut so) = (r.command_offset(), s.command_offset());
                    assert_ne!(ro, so, "return with identical sources");
                    if ro < so {
                        so -= 1;
                    }

                    res.push(0x6c + ro * 5 + so);
                    if let Readable::MemAddr(addr) = r {
                        if let MemAddr::Byte(v) = addr {
                            res.push(v);
                        } else {
                            unreachable!("ljmp with named mem addr");
                        }
                    } else if let Readable::MemAddr(addr) = s {
                        if let MemAddr::Byte(v) = addr {
                            res.push(v);
                        } else {
                            unreachable!("ljmp with named mem addr");
                        }
                    }
                }
                Command::Section(_) => (),
                cmd @ Command::Invalid | cmd @ Command::If(_, _) => {
                    unreachable!("unexpected command: {:?}", cmd)
                }
            };
        }
    }

    res
}
