use std::ops::Range;

use diagnostics::Meta;

use hir::expr::Binop as HirBinop;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    /// `mod`
    Module,
    /// `fn`
    Function,
    /// `use`,
    Use,
    /// `let`
    Let,
    /// `struct`
    Struct,
    /// `union`
    Union,
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
    /// `%`
    Rem,
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
    /// `||`
    Or,
    /// `&&`
    And,
}

impl Binop {
    /// the result must be greater than 0
    /// as 0 is used in case there is no previous
    /// binop
    pub fn priority(self) -> u32 {
        match self {
            Binop::Or => 8,
            Binop::And => 9,
            Binop::Gt | Binop::Gte | Binop::Eq | Binop::Neq | Binop::Lte | Binop::Lt => 10,
            Binop::BitOr => 20,
            Binop::BitAnd => 21,
            Binop::Shl | Binop::Shr => 30,
            Binop::Add | Binop::Sub => 35,
            Binop::Mul | Binop::Div | Binop::Rem => 40,
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
                crate::Expression::Binop((), meta.replace(HirBinop::Add), a.into(), b.into())
            }
            Binop::Sub => {
                crate::Expression::Binop((), meta.replace(HirBinop::Sub), a.into(), b.into())
            }
            Binop::Mul => {
                crate::Expression::Binop((), meta.replace(HirBinop::Mul), a.into(), b.into())
            }
            Binop::Div => {
                crate::Expression::Binop((), meta.replace(HirBinop::Div), a.into(), b.into())
            }
            Binop::Rem => {
                crate::Expression::Binop((), meta.replace(HirBinop::Rem), a.into(), b.into())
            }
            Binop::Shl => {
                crate::Expression::Binop((), meta.replace(HirBinop::Shl), a.into(), b.into())
            }
            Binop::Shr => {
                crate::Expression::Binop((), meta.replace(HirBinop::Shr), a.into(), b.into())
            }
            Binop::Gt => {
                crate::Expression::Binop((), meta.replace(HirBinop::Gt), a.into(), b.into())
            }
            Binop::Gte => {
                crate::Expression::Binop((), meta.replace(HirBinop::Gte), a.into(), b.into())
            }
            Binop::Eq => {
                crate::Expression::Binop((), meta.replace(HirBinop::Eq), a.into(), b.into())
            }
            Binop::Neq => {
                crate::Expression::Binop((), meta.replace(HirBinop::Neq), a.into(), b.into())
            }
            Binop::Lte => {
                crate::Expression::Binop((), meta.replace(HirBinop::Lte), a.into(), b.into())
            }
            Binop::Lt => {
                crate::Expression::Binop((), meta.replace(HirBinop::Lt), a.into(), b.into())
            }
            Binop::BitOr => {
                crate::Expression::Binop((), meta.replace(HirBinop::BitOr), a.into(), b.into())
            }
            Binop::BitAnd => {
                crate::Expression::Binop((), meta.replace(HirBinop::BitAnd), a.into(), b.into())
            }
            Binop::Or | Binop::And => crate::desugar_logical_ops(self, meta.simplify(), a, b),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
    Integer(u128),
    Ident(&'a str),
    Keyword(Keyword),
    OpenBlock(BlockDelim),
    CloseBlock(BlockDelim),
    Binop(Binop),
    Invert,
    Assignment,
    SemiColon,
    Colon,
    /// `::`
    DoubleColon,
    Comma,
    Dot,
    Arrow,
    Underscore,
    /// `'name`
    Scope(&'a str),
    /// `@name`
    Attribute(&'a str),
    Invalid,
    EOF,
}

pub struct TokenIter<'a> {
    src: &'a str,
    file: &'a str,
    line: u32,
    byte_offset: usize,
}

impl<'a, 'b: 'a> TokenIter<'b> {
    pub fn new(src: &'b str, file: &'b str) -> Self {
        TokenIter {
            src,
            file,
            line: 1,
            byte_offset: 0,
        }
    }

    pub fn step_back(&mut self, undo: Meta<'a, Token<'a>>) {
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

    pub fn file(&self) -> &'b str {
        self.file
    }

    fn new_token(&self, tok: Token<'a>, origin: Range<usize>) -> Meta<'a, Token<'a>> {
        Meta {
            item: tok,
            source: self.src,
            file: self.file,
            span: origin,
            line: self.line,
        }
    }

    // is the character `c` a terminator for all token types
    fn is_terminator(&self, c: char) -> bool {
        c.is_whitespace()
            || match c {
                ';' | ':' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | '.' | '=' | '+' | '-'
                | '*' | '/' | '|' | '&' | '\'' | '@' | '>' | '<' => true,
                _ => false,
            }
    }

    fn current_char(&self) -> Option<char> {
        self.src[self.byte_offset..].chars().next()
    }

    /// returns an invalid token,
    /// recover never needs to skip a newline
    fn recover(&mut self, tok_start: usize) -> Meta<'a, Token<'a>> {
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

    fn parse_attr(&mut self) -> Meta<'a, Token<'a>> {
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
                    Token::Attribute(v)
                } else {
                    unimplemented!("invalid attribute");
                }
            })
        } else {
            self.recover(start)
        }
    }

    /// expects the first character to be `'`
    fn parse_char_or_scope(&mut self) -> Meta<'a, Token<'a>> {
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
    fn parse_ident(&mut self) -> Meta<'a, Token<'a>> {
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
            "mod" => self.new_token(Token::Keyword(Keyword::Module), origin),
            "fn" => self.new_token(Token::Keyword(Keyword::Function), origin),
            "use" => self.new_token(Token::Keyword(Keyword::Use), origin),
            "let" => self.new_token(Token::Keyword(Keyword::Let), origin),
            "struct" => self.new_token(Token::Keyword(Keyword::Struct), origin),
            "union" => self.new_token(Token::Keyword(Keyword::Union), origin),
            "match" => self.new_token(Token::Keyword(Keyword::Match), origin),
            "if" => self.new_token(Token::Keyword(Keyword::If), origin),
            "else" => self.new_token(Token::Keyword(Keyword::Else), origin),
            "loop" => self.new_token(Token::Keyword(Keyword::Loop), origin),
            "while" => self.new_token(Token::Keyword(Keyword::While), origin),
            "break" => self.new_token(Token::Keyword(Keyword::Break), origin),
            "return" => self.new_token(Token::Keyword(Keyword::Return), origin),
            v => self.new_token(Token::Ident(v), origin),
        }
    }

    fn parse_num(&mut self) -> Meta<'a, Token<'a>> {
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

    pub fn next_token(&mut self) -> Meta<'a, Token<'a>> {
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
                    if self.current_char().map(|c| c == ':').unwrap_or(false) {
                        self.advance();
                        self.new_token(Token::DoubleColon, self.byte_offset - 2..self.byte_offset)
                    } else {
                        self.new_token(Token::Colon, self.byte_offset - 1..self.byte_offset)
                    }
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
                '%' => {
                    self.advance();
                    self.new_token(
                        Token::Binop(Binop::Rem),
                        self.byte_offset - 1..self.byte_offset,
                    )
                }
                '|' => {
                    self.advance();
                    if self.current_char().map(|c| c == '|').unwrap_or(false) {
                        self.advance();
                        self.new_token(
                            Token::Binop(Binop::Or),
                            self.byte_offset - 2..self.byte_offset,
                        )
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
                        self.advance();
                        self.new_token(
                            Token::Binop(Binop::And),
                            self.byte_offset - 2..self.byte_offset,
                        )
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
                        self.new_token(Token::Invert, self.byte_offset - 1..self.byte_offset)
                    }
                }
                '\'' => self.parse_char_or_scope(),
                '@' => self.parse_attr(),
                _ => self.recover(self.byte_offset),
            }
        }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Meta<'a, Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token())
    }
}
