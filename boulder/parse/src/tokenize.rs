use diagnostics::Meta;

use std::{fmt, ops::Range};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    /// `fn`
    Function,
    /// `let`
    Let,
    /// `struct`
    Struct,
    /// `match`
    Match,
    /// `if`
    If,
    /// `else`
    Else,
    /// `loop`
    Loop,
    /// `while`
    While,
    /// `break`
    Break,
    /// `return`
    Return,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Keyword::Function => write!(f, "fn"),
            Keyword::Let => write!(f, "let"),
            Keyword::Struct => write!(f, "struct"),
            Keyword::Match => write!(f, "match"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::Loop => write!(f, "loop"),
            Keyword::While => write!(f, "while"),
            Keyword::Break => write!(f, "loop"),
            Keyword::Return => write!(f, "return"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BlockDelim {
    /// `()`
    Parenthesis,
    /// `[]`
    Bracket,
    /// `{}`
    Brace,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Binop {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
    /// `>`
    Gt,
    /// `>=`
    Gte,
    /// `==`
    Eq,
    /// `!=`
    Neq,
    /// `<=`
    Lte,
    /// `<`
    Lt,
    /// `|`
    BitOr,
    /// `&`
    BitAnd,
}

impl Binop {
    /// the result must be greater than 0
    /// as 0 is used in case there is no previous
    /// binop
    pub fn priority(self) -> u32 {
        match self {
            Binop::Gt | Binop::Gte | Binop::Eq | Binop::Neq | Binop::Lte | Binop::Lt => 10,
            Binop::BitOr => 20,
            Binop::BitAnd => 21,
            Binop::Shl | Binop::Shr => 30,
            Binop::Add => 35,
            Binop::Sub => 35,
            Binop::Mul => 40,
            Binop::Div => 40,
        }
    }

    pub fn as_hir_expr<'a, T>(
        self,
        meta: Meta<'a, T>,
        a: crate::Expression<'a>,
        b: crate::Expression<'a>,
    ) -> crate::Expression<'a> {
        match self {
            Binop::Add => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Add), a.into(), b.into())
            }
            Binop::Sub => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Sub), a.into(), b.into())
            }
            Binop::Mul => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Mul), a.into(), b.into())
            }
            Binop::Div => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Div), a.into(), b.into())
            }
            Binop::Shl => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Shl), a.into(), b.into())
            }
            Binop::Shr => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Shr), a.into(), b.into())
            }
            Binop::Gt => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Gt), a.into(), b.into())
            }
            Binop::Gte => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Gte), a.into(), b.into())
            }
            Binop::Eq => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Eq), a.into(), b.into())
            }
            Binop::Neq => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Neq), a.into(), b.into())
            }
            Binop::Lte => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Lte), a.into(), b.into())
            }
            Binop::Lt => {
                crate::Expression::Binop((), meta.replace(hir::Binop::Lt), a.into(), b.into())
            }
            Binop::BitOr => {
                crate::Expression::Binop((), meta.replace(hir::Binop::BitOr), a.into(), b.into())
            }
            Binop::BitAnd => {
                crate::Expression::Binop((), meta.replace(hir::Binop::BitAnd), a.into(), b.into())
            }
        }
    }
}

impl fmt::Display for Binop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Binop::Add => write!(f, "+"),
            Binop::Sub => write!(f, "-"),
            Binop::Mul => write!(f, "*"),
            Binop::Div => write!(f, "/"),
            Binop::Shl => write!(f, "<<"),
            Binop::Shr => write!(f, ">>"),
            Binop::Gt => write!(f, ">"),
            Binop::Gte => write!(f, ">="),
            Binop::Eq => write!(f, "=="),
            Binop::Neq => write!(f, "!="),
            Binop::Lte => write!(f, "<="),
            Binop::Lt => write!(f, "<"),
            Binop::BitOr => write!(f, "|"),
            Binop::BitAnd => write!(f, "&"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Integer(u128),
    Ident(Box<str>),
    Keyword(Keyword),
    OpenBlock(BlockDelim),
    CloseBlock(BlockDelim),
    Binop(Binop),
    Assignment,
    SemiColon,
    Colon,
    Comma,
    Dot,
    Arrow,
    Underscore,
    /// `'`
    Scope(Box<str>),
    Invalid,
    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Integer(v) => write!(f, "{}", v),
            Token::Ident(v) => write!(f, "{}", v),
            Token::Keyword(k) => write!(f, "{}", k),
            Token::OpenBlock(BlockDelim::Parenthesis) => write!(f, "("),
            Token::OpenBlock(BlockDelim::Bracket) => write!(f, "["),
            Token::OpenBlock(BlockDelim::Brace) => write!(f, "{{"),
            Token::CloseBlock(BlockDelim::Parenthesis) => write!(f, ")"),
            Token::CloseBlock(BlockDelim::Bracket) => write!(f, "]"),
            Token::CloseBlock(BlockDelim::Brace) => write!(f, "}}"),
            Token::Binop(o) => write!(f, "{}", o),
            Token::Assignment => write!(f, "="),
            Token::SemiColon => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Arrow => write!(f, "->"),
            Token::Underscore => write!(f, "_"),
            Token::Scope(v) => write!(f, "'{}", v),
            Token::Invalid => write!(f, "<invalid token>"),
            Token::EOF => write!(f, "<EOF>"),
        }
    }
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

    pub fn step_back(&mut self, undo: Meta<'a, Token>) {
        self.line = undo.line;
        self.byte_offset = undo.span.start;
    }

    pub fn current_line(&self) -> u32 {
        self.line
    }

    pub fn current_offset(&self) -> usize {
        self.byte_offset
    }

    pub fn source(&self) -> &'b str {
        self.src
    }

    fn new_token(&self, tok: Token, origin: Range<usize>) -> Meta<'a, Token> {
        Meta {
            item: tok,
            source: self.src,
            span: origin,
            line: self.line,
        }
    }

    // is the character `c` a terminator for all token types
    fn is_terminator(&self, c: char) -> bool {
        c.is_whitespace()
            || match c {
                ';' | ':' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | '.' | '=' | '+' | '-'
                | '*' | '/' | '|' | '&' | '\'' => true,
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

    /// expects the first character to be `'`
    fn parse_char_or_scope(&mut self) -> Meta<'a, Token> {
        let start = self.byte_offset;
        // TODO: accept char literal
        self.advance();
        if self
            .current_char()
            .map_or(false, |first| first.is_alphabetic() || first == '_')
        {
            let mut ident = self.parse_ident();
            ident.span.start -= 1;
            ident.map(|i| {
                if let Token::Ident(v) = i {
                    Token::Scope(v)
                } else {
                    unimplemented!("invalid scopes");
                }
            })
        } else {
            self.recover(start)
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

        let origin = start..self.byte_offset;
        match &self.src[start..self.byte_offset] {
            "_" => self.new_token(Token::Underscore, origin),
            "fn" => self.new_token(Token::Keyword(Keyword::Function), origin),
            "let" => self.new_token(Token::Keyword(Keyword::Let), origin),
            "struct" => self.new_token(Token::Keyword(Keyword::Struct), origin),
            "match" => self.new_token(Token::Keyword(Keyword::Match), origin),
            "if" => self.new_token(Token::Keyword(Keyword::If), origin),
            "else" => self.new_token(Token::Keyword(Keyword::Else), origin),
            "loop" => self.new_token(Token::Keyword(Keyword::Loop), origin),
            "while" => self.new_token(Token::Keyword(Keyword::While), origin),
            "break" => self.new_token(Token::Keyword(Keyword::Break), origin),
            "return" => self.new_token(Token::Keyword(Keyword::Return), origin),
            v => self.new_token(Token::Ident(v.into()), origin),
        }
    }

    fn parse_num(&mut self) -> Meta<'a, Token> {
        // get num str
        let s = self.src[self.byte_offset..]
            .split(|c: char| self.is_terminator(c))
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

    fn next_token(&mut self) -> Meta<'a, Token> {
        let first = if let Some(c) = self.current_char() {
            c
        } else {
            return self.new_token(Token::EOF, self.byte_offset..self.byte_offset);
        };

        if first.is_alphabetic() || first == '_' {
            self.parse_ident()
        } else if first.is_numeric() {
            self.parse_num()
        } else if first.is_whitespace() {
            self.advance();
            self.next_token()
        } else {
            match first {
                '#' => {
                    while self.current_char().map_or(false, |c| c != '\n') {
                        self.advance();
                    }
                    self.next_token()
                }
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
                '.' => {
                    self.advance();
                    self.new_token(Token::Dot, self.byte_offset - 1..self.byte_offset)
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
                        Token::Binop(Binop::Add),
                        self.byte_offset - 1..self.byte_offset,
                    )
                }
                '-' => {
                    self.advance();
                    if self.current_char().map(|c| c == '>').unwrap_or(false) {
                        self.advance();
                        self.new_token(Token::Arrow, self.byte_offset - 2..self.byte_offset)
                    } else {
                        self.new_token(
                            Token::Binop(Binop::Sub),
                            self.byte_offset - 1..self.byte_offset,
                        )
                    }
                }
                '*' => {
                    self.advance();
                    self.new_token(
                        Token::Binop(Binop::Mul),
                        self.byte_offset - 1..self.byte_offset,
                    )
                }
                '/' => {
                    self.advance();
                    self.new_token(
                        Token::Binop(Binop::Div),
                        self.byte_offset - 1..self.byte_offset,
                    )
                }
                '|' => {
                    self.advance();
                    if self.current_char().map(|c| c == '|').unwrap_or(false) {
                        unimplemented!()
                    } else {
                        self.new_token(
                            Token::Binop(Binop::BitOr),
                            self.byte_offset - 1..self.byte_offset,
                        )
                    }
                }
                '&' => {
                    self.advance();
                    if self.current_char().map(|c| c == '&').unwrap_or(false) {
                        unimplemented!()
                    } else {
                        self.new_token(
                            Token::Binop(Binop::BitAnd),
                            self.byte_offset - 1..self.byte_offset,
                        )
                    }
                }
                '=' => {
                    self.advance();
                    if self.current_char().map_or(false, |c| c == '=') {
                        self.advance();
                        self.new_token(
                            Token::Binop(Binop::Eq),
                            self.byte_offset - 2..self.byte_offset,
                        )
                    } else {
                        self.new_token(Token::Assignment, self.byte_offset - 1..self.byte_offset)
                    }
                }
                '>' => {
                    self.advance();
                    if self.current_char().map_or(false, |c| c == '>') {
                        self.advance();
                        self.new_token(
                            Token::Binop(Binop::Shr),
                            self.byte_offset - 2..self.byte_offset,
                        )
                    } else if self.current_char().map_or(false, |c| c == '=') {
                        self.advance();
                        self.new_token(
                            Token::Binop(Binop::Gte),
                            self.byte_offset - 2..self.byte_offset,
                        )
                    } else {
                        self.new_token(
                            Token::Binop(Binop::Gt),
                            self.byte_offset - 1..self.byte_offset,
                        )
                    }
                }
                '<' => {
                    self.advance();
                    if self.current_char().map_or(false, |c| c == '<') {
                        self.advance();
                        self.new_token(
                            Token::Binop(Binop::Shl),
                            self.byte_offset - 2..self.byte_offset,
                        )
                    } else if self.current_char().map_or(false, |c| c == '=') {
                        self.advance();
                        self.new_token(
                            Token::Binop(Binop::Lte),
                            self.byte_offset - 2..self.byte_offset,
                        )
                    } else {
                        self.new_token(
                            Token::Binop(Binop::Lt),
                            self.byte_offset - 1..self.byte_offset,
                        )
                    }
                }
                '!' => {
                    self.advance();
                    if self.current_char().map_or(false, |c| c == '=') {
                        self.advance();
                        self.new_token(
                            Token::Binop(Binop::Neq),
                            self.byte_offset - 2..self.byte_offset,
                        )
                    } else {
                        unimplemented!()
                    }
                }
                '\'' => self.parse_char_or_scope(),
                _ => self.recover(self.byte_offset),
            }
        }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Meta<'a, Token>;

    fn next(&mut self) -> Option<Meta<'a, Token>> {
        Some(self.next_token())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scope() {
        assert_eq!(
            Token::Scope("a".into()),
            TokenIter::new("'a").next().unwrap().item
        );
    }
}
