use boulder_core::{CompileError, Meta};
use hir::HIR;

use std::mem;

mod tokenize;

use tokenize::{BlockDelim, Keyword, Token, TokenIter};

struct Scope(&'static str);

macro_rules! scope {
    ($l:literal) => {
        println!("entering scope: {}", $l);
        let _s = Scope($l);
    };
}

impl Drop for Scope {
    fn drop(&mut self) {
        println!("exiting scope: {}", self.0);
    }
}

pub fn parse(src: &str) -> Result<HIR, CompileError> {
    let iter = &mut TokenIter::new(src);
    let mut hir = HIR::new();
    while let Some(mut token) = iter.next() {
        match mem::replace(&mut token.item, Token::Invalid) {
            Token::Keyword(Keyword::Function) => {
                hir.add_function(parse_function(iter)?);
            }
            Token::EOF => break,
            _ => CompileError::expected(&[Token::Keyword(Keyword::Function), Token::EOF], &token)?,
        }
    }

    Ok(hir)
}

fn consume_token(expected: Token, iter: &mut TokenIter) -> Result<(), CompileError> {
    let tok = iter.next().unwrap();
    if expected == tok.item {
        Ok(())
    } else {
        CompileError::expected(&expected, &tok)
    }
}

fn expect_ident(tok: Meta<'_, Token>) -> Result<Box<str>, CompileError> {
    if let Token::Ident(value) = tok.item {
        Ok(value)
    } else {
        CompileError::expected(&"Ident", &tok)
    }
}

fn parse_variable_decl<'a>(_iter: &mut TokenIter<'a>) -> Result<hir::Expression<'a>, CompileError> {
    unimplemented!("variable declaration");
}

/// parse the rhs of a binary operation, each binop must have a priority greater than `priority`.
fn parse_binop_rhs<'a>(priority: u32, iter: &mut TokenIter<'a>) -> Result<hir::Expression<'a>, CompileError> {
    scope!("parse_binop_rhs");
    let mut start = iter.next().unwrap();
    let mut expr = match mem::replace(&mut start.item, Token::Invalid) {
        Token::Ident(v) => hir::Expression::Variable(start.simplify(), v),
        Token::OpenBlock(BlockDelim::Parenthesis) => {
            let expr = parse_expression(iter.next().unwrap(), iter)?;
            consume_token(Token::CloseBlock(BlockDelim::Parenthesis), iter)?;
            expr
        }
        _ => CompileError::expected(&[Token::Ident("".into())], &start)?,
    };

    let next = iter.next().unwrap();
    match &next.item {
        Token::Operator(_) => {
            let mut next = next;
            while let Token::Operator(op) = next.item {
                if op.priority() > priority {
                    expr = op.as_hir_expr(next, expr, parse_binop_rhs(op.priority(), iter)?);
                } else {
                    break;
                }
                next = iter.next().unwrap();
            }
            iter.step_back(next);
            Ok(expr)
        }
        _ => {
            iter.step_back(next);
            Ok(expr)
        }
    }
}

fn parse_expression<'a>(
    mut start: Meta<'a, Token>,
    iter: &mut TokenIter<'a>,
) -> Result<hir::Expression<'a>, CompileError> {
    scope!("parse_expr");
    match mem::replace(&mut start.item, Token::Invalid) {
        Token::OpenBlock(BlockDelim::Brace) => parse_block(iter),
        Token::Keyword(Keyword::Let) => parse_variable_decl(iter),
        Token::Ident(v) => {
            let mut next = iter.next().unwrap();
            match &next.item {
                Token::Assignment => {
                    let start = iter.next().unwrap();
                    Ok(hir::Expression::Assignment(
                        next.replace(v.into()),
                        Box::new(parse_expression(start, iter)?),
                    ))
                }
                Token::Operator(_) => {
                    let mut expr = hir::Expression::Variable(next.simplify(), v);
                    while let Token::Operator(op) = next.item {
                        expr = op.as_hir_expr(next.simplify(), expr, parse_binop_rhs(op.priority(), iter)?);
                        next = iter.next().unwrap();
                    }
                    iter.step_back(next);
                    Ok(expr)
                }
                _ => {
                    iter.step_back(next);
                    Ok(hir::Expression::Variable(start.simplify(), v))
                }
            }
        }
        _ => CompileError::expected(&[Token::OpenBlock(BlockDelim::Brace)], &start),
    }
}

fn parse_block<'a>(iter: &mut TokenIter<'a>) -> Result<hir::Expression<'a>, CompileError> {
    let mut block = Vec::new();
    let start = iter.current_offset();
    let line = iter.current_line();
    loop {
        let tok = iter.next().unwrap();
        if tok.item == Token::CloseBlock(BlockDelim::Brace) {
            let end = iter.current_offset();
            return Ok(hir::Expression::Block(Meta::empty(iter.source(), line, start..end), block));
        }

        block.push(parse_expression(tok, iter)?);
    }
}

// parse a function, `fn` should already be consumed
fn parse_function<'a>(iter: &mut TokenIter<'a>) -> Result<hir::Function<'a>, CompileError> {
    let mut func = hir::Function::new(expect_ident(iter.next().unwrap())?);

    consume_token(Token::OpenBlock(BlockDelim::Parenthesis), iter)?;
    loop {
        let tok = iter.next().unwrap();
        if tok.item == Token::CloseBlock(BlockDelim::Parenthesis) {
            break;
        }

        let arg_name = expect_ident(tok)?;
        consume_token(Token::Colon, iter)?;
        let arg_type = expect_ident(iter.next().unwrap())?;
        func.add_argument(arg_name, arg_type);

        let tok = iter.next().unwrap();
        if tok.item == Token::CloseBlock(BlockDelim::Parenthesis) {
            break;
        } else if tok.item != Token::Comma {
            CompileError::expected(
                &[Token::Comma, Token::CloseBlock(BlockDelim::Parenthesis)],
                &tok,
            )?;
        }
    }

    let tok = iter.next().unwrap();
    if tok.item == Token::Arrow {
        func.set_ret(expect_ident(iter.next().unwrap())?);
        consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
    } else if tok.item != Token::OpenBlock(BlockDelim::Brace) {
        CompileError::expected(&Token::OpenBlock(BlockDelim::Brace), &tok)?;
    }

    func.set_body(parse_block(iter)?);

    Ok(func)
}

#[cfg(test)]
mod tests {
    use super::*;

    const IDENTITY: &str = "fn identity(t: u32) -> u32 { t }";

    #[test]
    fn identity() {
        parse(IDENTITY).unwrap();
    }

    #[test]
    fn binop_chain() {
        parse("fn a(t: u32) -> u32 { t + t / t - (t - t * t) + t }").unwrap();
    }

    #[test]
    fn function() {
        parse(
            "fn added(a: u32, b: u32) -> u32 {
                a + basti
            }",
        )
        .unwrap();
    }
}
