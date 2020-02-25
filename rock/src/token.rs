use std::ops::Range;

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    Byte(u8),
    Ident,
    Section,
    A,
    B,
    C,
    D,
    Mem,
    BlockAddr,
    SectionAddr,
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
    pub fn origin(&self) -> &'a str {
        self.origin
    }

    pub fn ty(&self) -> TokenType {
        self.ty
    }

    pub fn line(&self) -> usize {
        self.line
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

    fn tok(&self, ty: TokenType, origin: Range<usize>) -> Token<'a> {
        Token {
            line: self.line,
            origin: &self.src[origin],
            ty,
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
        self.tok(TokenType::Invalid, tok_start..self.byte_offset)
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

        let ty = match &self.src[start..self.byte_offset] {
            "A" => TokenType::A,
            "B" => TokenType::B,
            "C" => TokenType::C,
            "D" => TokenType::D,
            "mem" => TokenType::Mem,
            "M1" => TokenType::SectionAddr,
            "M2" => TokenType::BlockAddr,
            sec => {
                if sec.starts_with('.') {
                    TokenType::Section
                } else {
                    TokenType::Ident
                }
            }
        };

        self.tok(ty, start..self.byte_offset)
    }

    fn parse_num(&'a mut self) -> Token<'b> {
        // get num str
        let start = self.byte_offset;
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
                self.tok(TokenType::Byte(v), start..self.byte_offset)
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
                Some(self.parse_ident())
            } else if first.is_numeric() {
                Some(self.parse_num())
            } else if first.is_whitespace() {
                self.advance();
                self.next()
            } else if first == ';' {
                self.advance();
                Some(self.tok(TokenType::SemiColon, self.byte_offset - 1..self.byte_offset))
            } else if first == ':' {
                self.advance();
                Some(self.tok(TokenType::Colon, self.byte_offset - 1..self.byte_offset))
            } else if first == '#' {
                while let Some(c) = self.current_char() {
                    if c == '\n' {
                        break;
                    } else {
                        self.byte_offset += c.len_utf8();
                    }
                }
                self.next()
            } else {
                Some(self.recover(self.byte_offset))
            }
        } else {
            None
        }
    }
}
