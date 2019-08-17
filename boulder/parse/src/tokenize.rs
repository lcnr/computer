use boulder_core::Meta;

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    /// `fn`
    Function,
    /// `let`
    Let,
}

#[derive(Debug, Clone, Copy)]
pub enum BlockDelim {
    /// `()`
    Parenthesis,
    /// `[]`
    Bracket,
    /// `{}`
    Brace,
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul
}

#[derive(Debug, Clone)]
pub enum Token {
    Integer(u128),
    Ident(Box<str>),
    Keyword(Keyword),
    OpenBlock(BlockDelim),
    Invalid,
    Operator(Operator),
}

pub struct TokenIter<'a> {
    src: &'a str,
    line: u32,
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

    fn new_token(&self, tok: Token, Range<usize>, line: u32) -> Meta<'a, Token> {
        Meta {
            data: tok,
            file: `
            origin: self.src,
            line,
            
        }
    }

    fn current_char(&self) -> Option<char> {
        self.src[self.byte_offset..].chars().next()
    }

    /// returns an invalid token,
    /// recover never needs to skip a newline
    fn recover(&'a mut self, tok_start: usize) -> Meta<'a, Token> {
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

        if self.current_char() == Some('#') {
            while let Some(c) = self.current_char() {
                if c == '\n' {
                    break;
                } else {
                    self.byte_offset += c.len_utf8();
                }
            }
        }
    }

    /// expects that the first byte is already checked to be alphabetical
    fn parse_ident(&mut self) -> Meta<'a, Token> {
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

    fn parse_num(&'a mut self) -> Meta<'a, Token> {
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

        match u128::from_str_radix(num_str, base) {
            Ok(v) => {
                self.byte_offset += s.len();
                Token::byte(self.line, s, v)
            }
            Err(_err) => self.recover(self.byte_offset),
        }
    }
}

impl<'b> Iterator for TokenIter<'b> {
    type Item = Meta<'b, Token>;

    fn next(&mut self) -> Option<Self::Item> {
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
