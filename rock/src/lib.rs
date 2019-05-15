#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    Byte(u8),
    Ident,
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
                }
                else {
                    return self.recover(start)
                }
            }
            self.advance();
        }
        
        Token::ident(self.line, &self.src[start..self.byte_offset])
    }

    fn parse_num(&'a mut self) -> Token<'b> {
        // get num str
        let s = self.src[self.byte_offset..].split(|c: char| c.is_whitespace() || c == ':' || c == ';').next().unwrap();

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
                _ => 10
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
            Err(_err) => {
                self.recover(self.byte_offset)
            }
        }
    }
}

impl<'b> Iterator for TokenIter<'b> {
    type Item = Token<'b>;

    fn next(&mut self) -> Option<Token<'b>> {
        if let Some(first) = self.current_char() {
            if first.is_alphabetic() || first == '.' || first == '_' {
                Some(self.parse_ident())
            } else if first.is_numeric() {
                Some(self.parse_num())
            } else if first.is_whitespace() {
                self.advance();
                self.next()
            } else if first == ';' {
                self.advance();
                Some(Token::semi_colon(self.line, &self.src[self.byte_offset - 1..self.byte_offset]))
            } else if first == ':' {
                self.advance();
                Some(Token::colon(self.line, &self.src[self.byte_offset - 1..self.byte_offset]))
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
    /// search for further errors
    Invalid,
    Section(&'a str),
}

impl<'a> Command<'a> {
    fn new<L: Logger>(curr: &mut Vec<Token<'a>>, l: &mut L) -> Self {
        unimplemented!()
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
        unimplemented!()
    }

    fn invalid() -> Self {
        Self {
            name: "<invalid>",
            pos: None,
            content: Vec::new()
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ErrorType {
    MissingStartBlock,
    UnknownCommand,
}

#[derive(Debug, Clone, Copy)]
enum ErrorLevel {
    Warn,
    Error,
}

#[derive(Debug, Clone)]
pub struct Error<'a> {
    lvl: ErrorLevel,
    ty: ErrorType,
    line: usize,
    origin: &'a str,
}

impl<'a> Error<'a> {
    fn new(lvl: ErrorLevel, ty: ErrorType, line: usize, origin: &'a str) -> Self {
        Self {
            lvl,
            ty,
            line,
            origin: origin
        }
    }

    fn from_token(lvl: ErrorLevel, ty: ErrorType, tok: &Token<'a>) -> Self {
        Self {
            lvl,
            ty,
            line: tok.line,
            origin: tok.origin
        }
    }

}

pub trait Logger {
    fn log_err(&mut self, src: &str, err: Error<'_>);
}

pub struct DebugLogger;

impl Logger for DebugLogger {
    fn log_err(&mut self, src: &str, err: Error<'_>) {
        println!("{:?}", err);
    }
}

pub fn parse<'a, L: Logger>(src: &'a str, l: &mut L) -> Vec<Block<'a>> {
    let mut success = true;

    let mut blocks = Vec::new();

    let mut curr = Vec::with_capacity(4);
    for token in TokenIter::new(src) {
        let ty = token.ty();
        curr.push(token);
        match ty {
            TokenType::Colon => blocks.push(Block::new(&mut curr, l)),
            TokenType::SemiColon => {
                let command = Command::new(&mut curr, l);
                if let Some(last) = blocks.last_mut() {
                    last.content.push(command);
                } else {
                    l.log_err(
                        src,
                        Error::new(ErrorLevel::Warn, ErrorType::MissingStartBlock, 1, src.split(';').next().unwrap())
                    );
                    let mut block = Block::invalid();
                    block.content.push(command);
                    blocks.push(block);
                }
            }
            _ => (),
        }
    }

    blocks
}
