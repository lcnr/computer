use diagnostics::{CompileError, Meta};

use std::mem;

mod tokenize;

use tokenize::{BlockDelim, Keyword, Operator, Token, TokenIter};

type Expression<'a> = hir::expression::Expression<'a, hir::UnresolvedVariable<'a>, ()>;
type Function<'a> = hir::Function<'a, hir::UnresolvedVariable<'a>, hir::UnresolvedType, ()>;
type Hir<'a> = hir::Hir<'a, hir::UnresolvedVariable<'a>, hir::UnresolvedType, ()>;

pub fn parse<'a>(
    src: &'a str,
) -> Result<Hir, CompileError> {
    let iter = &mut TokenIter::new(src);
    let mut hir = Hir::new();
    while let Some(mut token) = iter.next() {
        match mem::replace(&mut token.item, Token::Invalid) {
            Token::Keyword(Keyword::Function) => {
                hir.add_function(parse_function(iter)?)?;
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

fn expect_ident<'a>(tok: Meta<'a, Token>) -> Result<Meta<'a, Box<str>>, CompileError> {
    let tok = tok.map(|t| {
        if let Token::Ident(value) = t {
            value
        } else {
            "".into()
        }
    });
    if tok.item.is_empty() {
        CompileError::expected(&"Ident", &tok)
    } else {
        Ok(tok)
    }
}

fn check_expr_terminator<'a>(
    tok: Meta<'a, Token>,
    prev_checked: &[Token],
) -> Result<Meta<'a, Token>, CompileError> {
    match tok.item {
        Token::SemiColon | Token::CloseBlock(_) => Ok(tok),
        _ => {
            let mut expected = vec![Token::SemiColon, Token::CloseBlock(BlockDelim::Brace)];
            expected.extend_from_slice(prev_checked);
            CompileError::expected(&expected, &tok)
        }
    }
}

fn parse_variable_decl<'a>(iter: &mut TokenIter<'a>) -> Result<Expression<'a>, CompileError> {
    let name = expect_ident(iter.next().unwrap())?;
    // TODO: allow for variables with inferred type
    consume_token(Token::Colon, iter)?;
    let ty = expect_ident(iter.next().unwrap())?;
    consume_token(Token::Assignment, iter)?;
    let input = parse_expression(iter)?;
    Ok(Expression::Assignment((), 
        hir::UnresolvedVariable::Typed(name, ty),
        Box::new(input),
    ))
}

/// parse the rhs of a binary operation, each binop must have a priority greater than `priority`.
fn parse_binop_rhs<'a>(
    priority: u32,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    let mut start = iter.next().unwrap();
    let mut expr = match mem::replace(&mut start.item, Token::Invalid) {
        Token::Ident(v) => {
            Expression::Variable((), hir::UnresolvedVariable::Simple(start.replace(v)))
        }
        Token::Integer(c) => Expression::Lit((), start.replace(hir::Literal::Integer(c))),
        Token::OpenBlock(BlockDelim::Parenthesis) => {
            let expr = parse_expression(iter)?;
            consume_token(Token::CloseBlock(BlockDelim::Parenthesis), iter)?;
            expr
        }
        _ => CompileError::expected(
            &[
                Token::Ident("".into()),
                Token::Integer(0),
                Token::OpenBlock(BlockDelim::Parenthesis),
            ],
            &start,
        )?,
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

fn parse_binop<'a>(
    mut lhs: Expression<'a>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    let mut next = iter.next().unwrap();
    while let Token::Operator(op) = next.item {
        lhs = op.as_hir_expr(next.simplify(), lhs, parse_binop_rhs(op.priority(), iter)?);
        next = iter.next().unwrap();
    }

    match &next.item {
        _ => {
            iter.step_back(check_expr_terminator(
                next,
                &[Token::Operator(Operator::Add)],
            )?);
            Ok(lhs)
        }
    }
}

fn parse_expression<'a>(iter: &mut TokenIter<'a>) -> Result<Expression<'a>, CompileError> {
    let mut start = iter.next().unwrap();
    match mem::replace(&mut start.item, Token::Invalid) {
        Token::OpenBlock(BlockDelim::Brace) => parse_block(iter),
        Token::Keyword(Keyword::Let) => parse_variable_decl(iter),
        Token::Ident(v) => {
            let next = iter.next().unwrap();
            match &next.item {
                Token::Assignment => Ok(Expression::Assignment((), 
                    hir::UnresolvedVariable::Simple(next.replace(v.into())),
                    Box::new(parse_expression(iter)?),
                )),
                Token::Operator(_) => {
                    iter.step_back(next);
                    parse_binop(
                        Expression::Variable((), hir::UnresolvedVariable::Simple(
                            start.replace(v),
                        )),
                        iter,
                    )
                }
                _ => {
                    iter.step_back(check_expr_terminator(
                        next,
                        &[Token::Assignment, Token::Operator(Operator::Add)],
                    )?);
                    Ok(Expression::Variable((), hir::UnresolvedVariable::Simple(
                        start.replace(v),
                    )))
                }
            }
        }
        Token::Integer(c) => {
            let next = iter.next().unwrap();
            match &next.item {
                Token::Operator(_) => {
                    iter.step_back(next);
                    parse_binop(
                        Expression::Lit((), start.replace(hir::Literal::Integer(c))),
                        iter,
                    )
                }
                _ => {
                    iter.step_back(check_expr_terminator(
                        next,
                        &[Token::Assignment, Token::Operator(Operator::Add)],
                    )?);
                    Ok(Expression::Lit((), 
                        start.replace(hir::Literal::Integer(c)),
                    ))
                }
            }
        }
        _ => CompileError::expected(
            &[
                Token::OpenBlock(BlockDelim::Brace),
                Token::Keyword(Keyword::Let),
                Token::Ident("".into()),
                Token::Integer(0),
            ],
            &start,
        ),
    }
}

fn parse_block<'a>(iter: &mut TokenIter<'a>) -> Result<Expression<'a>, CompileError> {
    let mut block = Vec::new();
    let start = iter.current_offset();
    let line = iter.current_line();
    loop {
        let mut tok = iter.next().unwrap();

        while tok.item == Token::SemiColon {
            tok = iter.next().unwrap();
            if let Some(expr) = block.pop() {
                block.push(Expression::Statement((), tok.simplify(), Box::new(expr)));
            }
        }

        if tok.item == Token::CloseBlock(BlockDelim::Brace) {
            let end = iter.current_offset();
            return Ok(Expression::Block((), 
                Meta::empty(iter.source(), line, start..end),
                block,
            ));
        }

        iter.step_back(tok);
        block.push(parse_expression(iter)?);
    }
}

// parse a function, `fn` should already be consumed
fn parse_function<'a>(
    iter: &mut TokenIter<'a>,
) -> Result<Function<'a>, CompileError> {
    let mut func = Function::new(expect_ident(iter.next().unwrap())?);

    consume_token(Token::OpenBlock(BlockDelim::Parenthesis), iter)?;
    loop {
        let tok = iter.next().unwrap();
        if tok.item == Token::CloseBlock(BlockDelim::Parenthesis) {
            break;
        }

        let arg_name = expect_ident(tok)?;
        consume_token(Token::Colon, iter)?;
        let arg_type = expect_ident(iter.next().unwrap())?;
        func.add_argument(arg_name, arg_type.map(|t| hir::UnresolvedType::Named(t)))?;

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
        func.set_return(expect_ident(iter.next().unwrap())?);
        consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
    } else if tok.item != Token::OpenBlock(BlockDelim::Brace) {
        CompileError::expected(&Token::OpenBlock(BlockDelim::Brace), &tok)?;
    }

    func.set_body(parse_block(iter)?);

    Ok(func)
}
