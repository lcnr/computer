#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    Byte(u8),
    Ident,
    Section,
    Colon,
    SemiColon,
    Invalid,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    line: usize,
    origin: &'a str,
    ty: TokenType,
}

impl<'a> Token<'a> {
    fn byte(line: usize, origin: &'a str, value: u8) -> Self {
        Self {
            line,
            origin,
            ty: TokenType::Byte(value),
        }
    }

    fn ident(line: usize, origin: &'a str) -> Self {
        Self {
            line,
            origin,
            ty: TokenType::Ident,
        }
    }

    fn colon(line: usize, origin: &'a str) -> Self {
        Self {
            line,
            origin,
            ty: TokenType::Colon,
        }
    }

    fn semi_colon(line: usize, origin: &'a str) -> Self {
        Self {
            line,
            origin,
            ty: TokenType::SemiColon,
        }
    }

    fn invalid(line: usize, origin: &'a str) -> Self {
        Self {
            line,
            origin,
            ty: TokenType::Invalid,
        }
    }

    pub fn origin(&self) -> &'a str {
        self.origin
    }

    pub fn ty(&self) -> TokenType {
        self.ty
    }

    pub fn is_byte(&self) -> bool {
        match self.ty {
            TokenType::Byte(_) => true,
            _ => false,
        }
    }

    /// this method returns u8::MAX if `self.ty != TokenType::Byte`
    pub fn byte_value(&self) -> u8 {
        match self.ty {
            TokenType::Byte(v) => v,
            _ => std::u8::MAX,
        }
    }

    pub fn is_ident(&self) -> bool {
        match self.ty {
            TokenType::Ident => true,
            _ => false,
        }
    }

    pub fn is_section(&self) -> bool {
        match self.ty {
            TokenType::Section => true,
            _ => false,
        }
    }

    pub fn is_colon(&self) -> bool {
        match self.ty {
            TokenType::Colon => true,
            _ => false,
        }
    }

    pub fn is_semi_colon(&self) -> bool {
        match self.ty {
            TokenType::SemiColon => true,
            _ => false,
        }
    }

    pub fn is_invalid(&self) -> bool {
        match self.ty {
            TokenType::Invalid => true,
            _ => false,
        }
    }
}

pub struct TokenIter<'a> {
    src: &'a str,
    line: usize,
    byte_offset: usize,
}

impl<'a, 'b: 'a> TokenIter<'b> {
    pub fn new(src: &'b str) -> Self {
        TokenIter {
            src,
            line: 1,
            byte_offset: 0,
        }
    }

    fn current_char(&self) -> Option<char> {
        self.src[self.byte_offset..].chars().next()
    }

    /// returns an invalid token,
    /// recover never needs to skip a newline
    fn recover(&'a mut self, tok_start: usize) -> Token<'b> {
        while let Some(c) = self.current_char() {
            if c.is_whitespace() || c == ';' || c == ':' {
                break;
            }
            self.advance();
        }
        Token::invalid(self.line, &self.src[tok_start..self.byte_offset])
    }

    fn advance(&mut self) {
        if let Some(c) = self.current_char() {
            self.byte_offset += c.len_utf8();
            if c == '\n' {
                self.line += 1;
            }
        }
    }

    /// expects that the first byte is already checked to be alphabetical
    fn parse_ident(&'a mut self) -> Token<'b> {
        let start = self.byte_offset;
        self.advance();
        while let Some(c) = self.current_char() {
            if !(c.is_alphanumeric() || c == '_') {
                if c.is_whitespace() || c == ';' || c == ':' {
                    break;
                } else {
                    return self.recover(start);
                }
            }
            self.advance();
        }

        Token::ident(self.line, &self.src[start..self.byte_offset])
    }

    fn parse_num(&'a mut self) -> Token<'b> {
        // get num str
        let s = self.src[self.byte_offset..]
            .split(|c: char| c.is_whitespace() || c == ':' || c == ';')
            .next()
            .unwrap();

        // strip underscores without allocating a new String unless necessary.
        let s2;
        let mut num_str = if s.chars().any(|c| c == '_') {
            s2 = s.chars().filter(|&c| c != '_').collect::<String>();
            &s2
        } else {
            s
        };

        let base = if num_str.starts_with('0') && num_str.len() > 1 {
            match num_str.as_bytes()[1] {
                b'x' => 16,
                b'o' => 8,
                b'b' => 2,
                _ => 10,
            }
        } else {
            10
        };

        if base != 10 {
            num_str = &num_str[2..];
        }

        match u8::from_str_radix(num_str, base) {
            Ok(v) => {
                self.byte_offset += s.len();
                Token::byte(self.line, s, v)
            }
            Err(_err) => self.recover(self.byte_offset),
        }
    }
}

impl<'b> Iterator for TokenIter<'b> {
    type Item = Token<'b>;

    fn next(&mut self) -> Option<Token<'b>> {
        if let Some(first) = self.current_char() {
            if first.is_alphabetic() || first == '.' || first == '_' {
                let mut tok = self.parse_ident();
                if first == '.' && tok.is_ident() {
                    tok.ty = TokenType::Section;
                }
                Some(tok)
            } else if first.is_numeric() {
                Some(self.parse_num())
            } else if first.is_whitespace() {
                self.advance();
                self.next()
            } else if first == ';' {
                self.advance();
                Some(Token::semi_colon(
                    self.line,
                    &self.src[self.byte_offset - 1..self.byte_offset],
                ))
            } else if first == ':' {
                self.advance();
                Some(Token::colon(
                    self.line,
                    &self.src[self.byte_offset - 1..self.byte_offset],
                ))
            } else {
                Some(self.recover(self.byte_offset))
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum Command<'a> {
    Invalid,
    Section(&'a str, usize),
    Idle,
    Inv,
}

impl<'a> Command<'a> {
    fn new(curr: &mut Vec<Token<'a>>, l: &mut impl Logger) -> Self {
        if let Some(command) = curr.first() {
            if !command.is_ident() {
                l.log_err(Error::expected(vec![TokenType::Ident], command));
                return Command::Invalid;
            }

            match command.origin() {
                "idle" => {
                    if curr.len() != 1 {
                        l.log_err(Error::from_token(
                            ErrorLevel::Error,
                            ErrorType::WrongArgCount(command.origin(), 1, curr.len()),
                            command,
                        ));
                        Command::Invalid
                    } else {
                        Command::Idle
                    }
                }
                unknown => {
                    l.log_err(Error::new(
                        ErrorLevel::Error,
                        ErrorType::UnknownCommand(unknown),
                    ));
                    Command::Invalid
                }
            }
        } else {
            l.log_err(Error::new(ErrorLevel::Error, ErrorType::UnknownCommand("")));
            Command::Invalid
        }
    }

    fn section(curr: &mut Vec<Token<'a>>, l: &mut impl Logger) -> Self {
        if curr.len() == 1 {
            Command::Section(curr.first().unwrap().origin(), 0)
        } else {
            l.log_err(Error::from_token(
                ErrorLevel::Error,
                ErrorType::InvalidSection,
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
    content: Vec<Command<'a>>,
}

impl<'a> Block<'a> {
    fn new<L: Logger>(curr: &mut Vec<Token<'a>>, l: &mut L) -> Self {
        match curr.len() {
            2 => {
                let b = &curr[1];
                let a = &curr[0];
                if a.is_ident() {
                    if b.is_byte() {
                        Block {
                            name: a.origin(),
                            pos: Some(b.byte_value()),
                            content: Vec::new(),
                        }
                    } else {
                        l.log_err(Error::expected(vec![TokenType::Byte(0)], &b));
                        Block::invalid()
                    }
                } else {
                    l.log_err(Error::expected(vec![TokenType::Ident], &a));
                    Block::invalid()
                }
            }
            1 => {
                let ident = &curr[0];
                if ident.is_ident() {
                    Block {
                        name: ident.origin(),
                        pos: None,
                        content: Vec::new(),
                    }
                } else {
                    l.log_err(Error::expected(vec![TokenType::Ident], &ident));
                    Block::invalid()
                }
            }
            _ => {
                if let Some(tok) = curr.first() {
                    l.log_err(Error::from_token(
                        ErrorLevel::Error,
                        ErrorType::UnspecifiedError,
                        tok,
                    ));
                } else {
                    l.log_err(Error::new(ErrorLevel::Error, ErrorType::UnspecifiedError));
                }

                Block::invalid()
            }
        }
    }

    fn invalid() -> Self {
        Self {
            name: "<invalid>",
            pos: None,
            content: Vec::new(),
        }
    }

    fn push(&mut self, command: Command<'a>) {
        self.content.push(command)
    }
}

#[derive(Debug, Clone)]
enum ErrorType<'a> {
    MissingStartBlock,
    UnknownCommand(&'a str),
    InvalidToken {
        expected: Vec<TokenType>,
        found: TokenType,
    },
    InvalidSection,
    UnspecifiedError,
    WrongArgCount(&'a str, usize, usize),
}

#[derive(Debug, Clone, Copy)]
enum ErrorLevel {
    Warn,
    Error,
}

#[derive(Debug, Clone)]
pub struct Error<'a> {
    lvl: ErrorLevel,
    ty: ErrorType<'a>,
    meta: Option<(usize, &'a str)>,
}

impl<'a> Error<'a> {
    fn new(lvl: ErrorLevel, ty: ErrorType<'a>) -> Self {
        Self {
            lvl,
            ty,
            meta: None,
        }
    }

    fn with_meta(lvl: ErrorLevel, ty: ErrorType<'a>, line: usize, origin: &'a str) -> Self {
        Self {
            lvl,
            ty,
            meta: Some((line, origin)),
        }
    }

    fn from_token(lvl: ErrorLevel, ty: ErrorType<'a>, tok: &Token<'a>) -> Self {
        Self {
            lvl,
            ty,
            meta: Some((tok.line, tok.origin)),
        }
    }

    /// Error: expected ... found ...
    fn expected(expected: Vec<TokenType>, tok: &Token<'a>) -> Self {
        Self {
            lvl: ErrorLevel::Error,
            ty: ErrorType::InvalidToken {
                expected,
                found: tok.ty,
            },
            meta: Some((tok.line, tok.origin)),
        }
    }
}

fn current_block<'a, 'b: 'a>(
    blocks: &'a mut Vec<Block<'b>>,
    src: &'b str,
    l: &mut impl Logger,
) -> &'a mut Block<'b> {
    if blocks.is_empty() {
        l.log_err(Error::with_meta(
            ErrorLevel::Warn,
            ErrorType::MissingStartBlock,
            1,
            src.split(';').next().unwrap(),
        ));
        let block = Block::invalid();
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

pub fn parse<'a, L: Logger>(src: &'a str, l: &mut L) -> Vec<Block<'a>> {
    let mut blocks = Vec::new();

    let mut curr: Vec<Token> = Vec::with_capacity(4);
    for token in TokenIter::new(src) {
        let ty = token.ty();
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
            _ => curr.push(token),
        }
    }

    if !curr.is_empty() {
        let current_block = current_block(&mut blocks, src, l);
        let command = Command::new(&mut curr, l);
        current_block.push(command);
    }

    blocks
}
