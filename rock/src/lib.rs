#[derive(Debug, Clone)]
pub enum TokenType {
    Byte(u8),
    Ident,
    Colon,
    SemiColon,
    Invalid,
}

#[derive(Debug, Clone)]
pub struct Token {
    line: usize,
    byte_offset: usize,
    len: usize,
    ty: TokenType,
}

pub struct TokenIter<'a> {
    src: &'a str,
    line: usize,
    byte_offset: usize,
}

impl<'a> TokenIter<'a> {
    pub fn new(src: &'a str) -> Self {
        TokenIter {
            src,
            line: 0,
            byte_offset: 0,
        }
    }

    fn current_char(&self) -> Option<char> {
        self.src[self.byte_offset..].chars().next()
    }

    fn next_char(&self) -> Option<char> {
        self.src[self.byte_offset..].chars().skip(1).next()
    }

    fn recover(&mut self) -> usize {
        let mut skipped = 0;
        while let Some(c) = self.current_char() {
            if c.is_whitespace() || c == ';' || c == ':' {
                break;
            }
            self.advance();
            skipped += 1;
        }
        skipped
    }

    fn advance(&mut self) {
        if let Some(c) = self.next_char() {
            self.byte_offset += c.len_utf8();
            if c == '\n' {
                self.line += 1;
            }
        }
    }

    fn parse_ident(&mut self) -> Token {
        unimplemented!()
    }

    fn parse_num(&mut self) -> Token {
        unimplemented!()
    }
}

impl Iterator for TokenIter<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if let Some(first) = self.current_char() {
            if first.is_alphabetic() || first == '.' || first == '_' {
                Some(self.parse_ident())
            } else if first.is_numeric() {
                Some(self.parse_num())
            } else if first.is_whitespace() {
                self.advance();
                self.next()
            } else if first == ';' {
                let token = Token {
                    line: self.line,
                    byte_offset: self.byte_offset,
                    len: 1,
                    ty: TokenType::SemiColon,
                };
                self.advance();
                Some(token)
            } else if first == ':' {
                let token = Token {
                    line: self.line,
                    byte_offset: self.byte_offset,
                    len: 1,
                    ty: TokenType::Colon,
                };
                self.advance();
                Some(token)
            } else {
                let byte_offset = self.byte_offset;
                let len = self.recover();
                Some(Token {
                    line: self.line,
                    byte_offset: byte_offset,
                    len,
                    ty: TokenType::Invalid,
                })
            }
        } else {
            None
        }
    }
}
