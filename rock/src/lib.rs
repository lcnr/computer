use std::marker::PhantomData;

#[derive(Debug, Clone)]
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
    len: usize,
    byte_offset: usize,
    ty: TokenType,
    _phantom: PhantomData<&'a str>
}

impl<'a> Token<'a> {
    fn byte(line: usize, len: usize, byte_offset: usize, value: u8) -> Self {
        Self {
            line,
            len,
            byte_offset,
            ty: TokenType::Byte(value),
            _phantom: PhantomData
        }
    }

    fn ident(line: usize, len: usize, byte_offset: usize) -> Self {
        Self {
            line,
            len,
            byte_offset,
            ty: TokenType::Ident,
            _phantom: PhantomData
        }
    }

    fn colon(line: usize, len: usize, byte_offset: usize) -> Self {
        Self {
            line,
            len,
            byte_offset,
            ty: TokenType::Colon,
            _phantom: PhantomData
        }
    }

    fn semi_colon(line: usize, len: usize, byte_offset: usize) -> Self {
        Self {
            line,
            len,
            byte_offset,
            ty: TokenType::SemiColon,
            _phantom: PhantomData
        }
    }

    fn invalid(line: usize, len: usize, byte_offset: usize) -> Self {
        Self {
            line,
            len,
            byte_offset,
            ty: TokenType::Invalid,
            _phantom: PhantomData
        }
    }

    /// expects the original source
    pub fn content(&self, src: &'a str) -> &'a str {
        &src[self.byte_offset..self.byte_offset + self.len]
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
            line: 0,
            byte_offset: 0,
        }
    }

    fn current_char(&self) -> Option<char> {
        self.src[self.byte_offset..].chars().next()
    }

    /// returns an invalid token,
    /// recover never needs to skip a newline
    fn recover(&'a mut self, tok_start: usize) -> Token<'b> {
        let mut skipped = self.byte_offset - tok_start;
        while let Some(c) = self.current_char() {
            if c.is_whitespace() || c == ';' || c == ':' {
                break;
            }
            self.advance();
            skipped += 1;
        }
        Token::invalid(self.line, skipped, tok_start)
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
        
        Token::ident(self.line, self.byte_offset - start, start)
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
                Token::byte(self.line, s.len(), self.byte_offset - s.len(), v)
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
                let token = Token::semi_colon(self.line, 1, self.byte_offset);
                self.advance();
                Some(token)
            } else if first == ':' {
                let token = Token::colon(self.line, 1, self.byte_offset);
                self.advance();
                Some(token)
            } else {
                Some(self.recover(self.byte_offset))
            }
        } else {
            None
        }
    }
}
