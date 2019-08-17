use boulder_core::Meta;

use std::ops::Range;

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
    Mul,
}

#[derive(Debug, Clone)]
pub enum Token {
    Integer(u128),
    Ident(Box<str>),
    Keyword(Keyword),
    OpenBlock(BlockDelim),
    CloseBlock(BlockDelim),
    Operator(Operator),
    SemiColon,
    Colon,
    Comma,
    Invalid,
    Arrow,
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

    fn new_token(&self, tok: Token, origin: Range<usize>) -> Meta<'a, Token> {
        Meta {
            data: tok,
            source: self.src,
            span: origin,
            line: self.line,
        }
    }

    // is the character `c` a terminator for all token types
    fn is_terminator(&self, c: char) -> bool {
        c.is_whitespace()
            || match c {
                ';' | ':' | '(' | ')' | '{' | '}' | '[' | ']' | '.' | ',' => true,
                _ => false,
            }
    }

    fn current_char(&self) -> Option<char> {
        self.src[self.byte_offset..].chars().next()
    }

    /// returns an invalid token,
    /// recover never needs to skip a newline
    fn recover(&mut self, tok_start: usize) -> Meta<'a, Token> {
        while let Some(c) = self.current_char() {
            if self.is_terminator(c) {
                println!("{}", c);
                break;
            }
            self.advance();
        }

        self.new_token(Token::Invalid, tok_start..self.byte_offset)
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
                if self.is_terminator(c) {
                    break;
                } else {
                    return self.recover(start);
                }
            }
            self.advance();
        }

        match &self.src[start..self.byte_offset] {
            "fn" => self.new_token(Token::Keyword(Keyword::Function), start..self.byte_offset),
            v => self.new_token(Token::Ident(v.into()), start..self.byte_offset),
        }
    }

    fn parse_num(&mut self) -> Meta<'a, Token> {
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
                self.new_token(
                    Token::Integer(v),
                    self.byte_offset - s.len()..self.byte_offset,
                )
            }
            Err(_err) => self.recover(self.byte_offset),
        }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Meta<'a, Token>;

    fn next(&mut self) -> Option<Meta<'a, Token>> {
        let first = self.current_char()?;
        Some(if first.is_alphabetic() || first == '_' {
            self.parse_ident()
        } else if first.is_numeric() {
            self.parse_num()
        } else if first.is_whitespace() {
            self.advance();
            self.next()?
        } else {
            match first {
                ';' => {
                    self.advance();
                    self.new_token(Token::SemiColon, self.byte_offset - 1..self.byte_offset)
                }
                ':' => {
                    self.advance();
                    self.new_token(Token::Colon, self.byte_offset - 1..self.byte_offset)
                }
                ',' => {
                    self.advance();
                    self.new_token(Token::Comma, self.byte_offset - 1..self.byte_offset)
                }
                '(' => {
                    self.advance();
                    self.new_token(
                        Token::OpenBlock(BlockDelim::Parenthesis),
                        self.byte_offset - 1..self.byte_offset,
                    )
                }
                '[' => {
                    self.advance();
                    self.new_token(
                        Token::OpenBlock(BlockDelim::Bracket),
                        self.byte_offset - 1..self.byte_offset,
                    )
                }
                '{' => {
                    self.advance();
                    self.new_token(
                        Token::OpenBlock(BlockDelim::Brace),
                        self.byte_offset - 1..self.byte_offset,
                    )
                }
                '}' => {
                    self.advance();
                    self.new_token(
                        Token::CloseBlock(BlockDelim::Brace),
                        self.byte_offset - 1..self.byte_offset,
                    )
                }
                ']' => {
                    self.advance();
                    self.new_token(
                        Token::CloseBlock(BlockDelim::Bracket),
                        self.byte_offset - 1..self.byte_offset,
                    )
                }
                ')' => {
                    self.advance();
                    self.new_token(
                        Token::CloseBlock(BlockDelim::Parenthesis),
                        self.byte_offset - 1..self.byte_offset,
                    )
                }
                '+' => {
                    self.advance();
                    self.new_token(
                        Token::Operator(Operator::Add),
                        self.byte_offset - 1..self.byte_offset,
                    )
                }
                '-' => {
                    self.advance();
                    if self.current_char().map(|c| c == '>').unwrap_or(false) {
                        self.advance();
                        self.new_token(
                            Token::Arrow,
                            self.byte_offset - 2..self.byte_offset,
                        )
                    } else {
                        self.new_token(
                            Token::Operator(Operator::Sub),
                            self.byte_offset - 1..self.byte_offset,
                        )
                    }
                }
                _ => self.recover(self.byte_offset),
            }
        })
    }
}
