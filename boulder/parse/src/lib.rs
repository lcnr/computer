use diagnostics::{CompileError, Meta};

use std::mem;

mod tokenize;

use tokenize::{BlockDelim, Keyword, Operator, Token, TokenIter};

type Expression<'a> =
    hir::expr::Expression<'a, hir::UnresolvedIdentifiers<'a>, hir::UnresolvedTypes<'a>>;
type Function<'a> = hir::Function<
    'a,
    hir::UnresolvedIdentifiers<'a>,
    hir::UnresolvedTypes<'a>,
    Option<hir::UnresolvedType<'a>>,
>;
type Hir<'a> = hir::Hir<
    'a,
    hir::UnresolvedIdentifiers<'a>,
    hir::UnresolvedTypes<'a>,
    Option<hir::UnresolvedType<'a>>,
    hir::UnresolvedType<'a>,
>;
type Type<'a> = hir::Type<'a, hir::UnresolvedType<'a>>;
type Kind<'a> = hir::ty::Kind<'a, hir::UnresolvedType<'a>>;
type Field<'a> = hir::ty::Field<'a, hir::UnresolvedType<'a>>;
type MatchArm<'a> =
    hir::expr::MatchArm<'a, hir::UnresolvedIdentifiers<'a>, hir::UnresolvedTypes<'a>>;

pub fn parse<'a>(src: &'a str) -> Result<Hir, CompileError> {
    let iter = &mut TokenIter::new(src);
    let mut hir = Hir::new();
    while let Some(mut token) = iter.next() {
        match mem::replace(&mut token.item, Token::Invalid) {
            Token::Keyword(Keyword::Function) => {
                hir.add_function(parse_function(iter)?)?;
            }
            Token::Keyword(Keyword::Struct) => {
                hir.add_type(parse_struct_decl(iter)?)?;
            }
            Token::EOF => break,
            _ => CompileError::expected(&[Token::Keyword(Keyword::Function), Token::EOF], &token)?,
        }
    }

    Ok(hir)
}

fn peek_token(expected: Token, iter: &mut TokenIter) -> bool {
    let tok = iter.next().unwrap();
    if expected == tok.item {
        iter.step_back(tok);
        true
    } else {
        iter.step_back(tok);
        false
    }
}

fn try_consume_token(expected: Token, iter: &mut TokenIter) -> bool {
    let tok = iter.next().unwrap();
    if expected == tok.item {
        true
    } else {
        iter.step_back(tok);
        false
    }
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

fn parse_pattern<'a>(iter: &mut TokenIter<'a>) -> Result<Meta<'a, Box<str>>, CompileError> {
    expect_ident(iter.next().unwrap())
}

fn check_expr_terminator<'a>(
    tok: Meta<'a, Token>,
    prev_checked: &[Token],
) -> Result<Meta<'a, Token>, CompileError> {
    match tok.item {
        Token::Comma
        | Token::SemiColon
        | Token::CloseBlock(_)
        | Token::OpenBlock(BlockDelim::Brace) => Ok(tok),
        _ => {
            let mut expected = vec![
                Token::SemiColon,
                Token::CloseBlock(BlockDelim::Brace),
                Token::OpenBlock(BlockDelim::Brace),
            ];
            expected.extend_from_slice(prev_checked);
            CompileError::expected(&expected, &tok)
        }
    }
}

fn parse_type<'a>(
    iter: &mut TokenIter<'a>,
) -> Result<Meta<'a, hir::UnresolvedType<'a>>, CompileError> {
    let first = expect_ident(iter.next().unwrap())?;
    let next = iter.next().unwrap();
    if let Token::Operator(Operator::BitOr) = &next.item {
        let mut parts = vec![first];
        let mut next = next;
        while let Token::Operator(Operator::BitOr) = &next.item {
            let component = expect_ident(iter.next().unwrap())?;
            parts.push(component);
            next = iter.next().unwrap();
        }
        iter.step_back(next);
        let meta = parts
            .first()
            .unwrap()
            .simplify()
            .append(parts.last().unwrap().simplify());
        Ok(meta.replace(hir::UnresolvedType::Sum(parts)))
    } else {
        iter.step_back(next);
        Ok(first.map(|f| hir::UnresolvedType::Named(f)))
    }
}

fn parse_ident_expr<'a>(
    ident: Meta<'a, Box<str>>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    let next = iter.next().unwrap();
    Ok(match next.item {
        Token::OpenBlock(BlockDelim::Parenthesis) => {
            let mut args = Vec::new();
            while !try_consume_token(Token::CloseBlock(BlockDelim::Parenthesis), iter) {
                args.push(parse_expression(iter)?);

                if !try_consume_token(Token::Comma, iter)
                    && !peek_token(Token::CloseBlock(BlockDelim::Parenthesis), iter)
                {
                    CompileError::expected(
                        &[Token::Comma, Token::CloseBlock(BlockDelim::Parenthesis)],
                        &iter.next().unwrap(),
                    )?;
                }
            }
            Expression::FunctionCall((), ident, args)
        }
        Token::Colon => {
            let var = Expression::Variable((), hir::UnresolvedVariable::Existing(ident));
            Expression::TypeRestriction(Box::new(var), parse_type(iter)?)
        }
        _ => {
            iter.step_back(next);
            Expression::Variable((), hir::UnresolvedVariable::Existing(ident))
        }
    })
}

fn parse_match<'a>(
    keyword: Meta<'a, ()>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    let value = parse_expression(iter)?;
    consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
    let mut match_arms = Vec::new();
    loop {
        if try_consume_token(Token::CloseBlock(BlockDelim::Brace), iter) {
            break;
        }

        let pat = parse_pattern(iter)?;
        consume_token(Token::Colon, iter)?;
        let ty = parse_type(iter)?;
        consume_token(Token::Arrow, iter)?;
        let expr = parse_expression(iter)?;

        match_arms.push(MatchArm {
            pattern: hir::UnresolvedVariable::New(pat, ty.map(|t| Some(t))),
            expr,
        });

        if try_consume_token(Token::CloseBlock(BlockDelim::Brace), iter) {
            break;
        } else {
            consume_token(Token::Comma, iter)?
        }
    }

    Ok(Expression::Match((), keyword, Box::new(value), match_arms))
}

fn parse_variable_decl<'a>(iter: &mut TokenIter<'a>) -> Result<Expression<'a>, CompileError> {
    let name = expect_ident(iter.next().unwrap())?;

    let ty = if try_consume_token(Token::Colon, iter) {
        Some(parse_type(iter)?)
    } else {
        None
    }
    .map_or_else(|| name.simplify().replace(None), |v| v.map(|t| Some(t)));

    if try_consume_token(Token::Assignment, iter) {
        let input = parse_expression(iter)?;
        Ok(Expression::Assignment(
            (),
            hir::UnresolvedVariable::New(name, ty),
            Box::new(input),
        ))
    } else {
        let tok = iter.next().unwrap();
        iter.step_back(check_expr_terminator(tok, &[Token::Assignment])?);
        Ok(Expression::Variable(
            (),
            hir::UnresolvedVariable::New(name, ty),
        ))
    }
}

/// parse the rhs of a binary operation, each binop must have a priority greater than `priority`.
fn parse_binop_rhs<'a>(
    priority: u32,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    let mut start = iter.next().unwrap();
    let mut expr = match mem::replace(&mut start.item, Token::Invalid) {
        Token::Scope(v) => {
            consume_token(Token::Colon, iter)?;
            let next = iter.next().unwrap();
            match next.item {
                Token::OpenBlock(BlockDelim::Brace) => {
                    let block = parse_block(Some(start.map(|_| v)), iter)?;
                    parse_binop(block, iter)
                }
                _ => CompileError::expected(&[Token::OpenBlock(BlockDelim::Brace)], &next),
            }?
        }
        Token::Ident(v) => parse_ident_expr(start.replace(v), iter)?,
        Token::Integer(c) => Expression::Lit((), start.replace(hir::Literal::Integer(c))),
        Token::Keyword(Keyword::Match) => parse_match(start.simplify(), iter)?,
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
    Ok(match &next.item {
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
            expr
        }
        Token::Dot => {
            Expression::FieldAccess((), Box::new(expr), expect_ident(iter.next().unwrap())?)
        }
        Token::Colon => Expression::TypeRestriction(Box::new(expr), parse_type(iter)?),
        _ => {
            iter.step_back(next);
            expr
        }
    })
}

fn parse_binop<'a>(
    mut lhs: Expression<'a>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    let mut next = iter.next().unwrap();
    loop {
        match next.item {
            Token::Operator(op) => {
                lhs = op.as_hir_expr(next.simplify(), lhs, parse_binop_rhs(op.priority(), iter)?);
                next = iter.next().unwrap();
            }
            Token::Dot => {
                lhs =
                    Expression::FieldAccess((), Box::new(lhs), expect_ident(iter.next().unwrap())?);
                next = iter.next().unwrap();
            }
            Token::Colon => {
                lhs = Expression::TypeRestriction(Box::new(lhs), parse_type(iter)?);
                next = iter.next().unwrap();
            }
            _ => break,
        }
    }

    match &next.item {
        _ => {
            iter.step_back(check_expr_terminator(
                next,
                &[Token::Operator(Operator::Add), Token::Dot, Token::Colon],
            )?);
            Ok(lhs)
        }
    }
}

fn parse_expression<'a>(iter: &mut TokenIter<'a>) -> Result<Expression<'a>, CompileError> {
    let mut start = iter.next().unwrap();
    match mem::replace(&mut start.item, Token::Invalid) {
        Token::OpenBlock(BlockDelim::Brace) => {
            let block = parse_block(None, iter)?;
            parse_binop(block, iter)
        }
        Token::Scope(v) => {
            consume_token(Token::Colon, iter)?;
            let next = iter.next().unwrap();
            match next.item {
                Token::OpenBlock(BlockDelim::Brace) => {
                    let block = parse_block(Some(start.map(|_| v)), iter)?;
                    parse_binop(block, iter)
                }
                _ => CompileError::expected(&[Token::OpenBlock(BlockDelim::Brace)], &start),
            }
        }
        Token::OpenBlock(BlockDelim::Parenthesis) => {
            let expr = parse_expression(iter)?;
            consume_token(Token::CloseBlock(BlockDelim::Parenthesis), iter)?;
            parse_binop(expr, iter)
        }
        Token::Keyword(Keyword::Let) => parse_variable_decl(iter),
        Token::Keyword(Keyword::Match) => {
            let expr = parse_match(start.simplify(), iter)?;
            parse_binop(expr, iter)
        }

        Token::Keyword(Keyword::Break) => {
            let mut next = iter.next().unwrap();
            let scope = if let Token::Scope(v) = mem::replace(&mut next.item, Token::Invalid) {
                next.replace(Some(v))
            } else {
                iter.step_back(next);
                start.replace(None)
            };
            Ok(Expression::Break(
                (),
                scope,
                Box::new(parse_expression(iter)?),
            ))
        }
        Token::Ident(v) => {
            let expr = parse_ident_expr(start.replace(v), iter)?;
            let next = iter.next().unwrap();
            Ok(match &next.item {
                Token::Assignment => {
                    if let Expression::Variable((), var) = expr {
                        Expression::Assignment((), var, Box::new(parse_expression(iter)?))
                    } else {
                        Err(
                            check_expr_terminator(next, &[Token::Operator(Operator::Add)])
                                .unwrap_err(),
                        )?
                    }
                }
                _ => {
                    iter.step_back(next);
                    parse_binop(expr, iter)?
                }
            })
        }
        Token::Integer(c) => {
            let expr = Expression::Lit((), start.replace(hir::Literal::Integer(c)));
            parse_binop(expr, iter)
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

fn parse_block<'a>(
    name: Option<Meta<'a, Box<str>>>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    let mut block = Vec::new();
    let start = iter.current_offset();
    let line = iter.current_line();
    loop {
        let mut tok = iter.next().unwrap();

        while tok.item == Token::SemiColon {
            tok = iter.next().unwrap();
            if let Some(expr) = block.pop() {
                block.push(Expression::Statement((), Box::new(expr)));
            }
        }

        if tok.item == Token::CloseBlock(BlockDelim::Brace) {
            let end = iter.current_offset();
            return Ok(Expression::Block(
                (),
                name.map_or_else(
                    || {
                        Meta::empty(iter.source(), line, start..end)
                            .map(|_| None)
                            .extend_left('{')
                    },
                    |v| v.map(|v| Some(v)),
                ),
                block,
            ));
        }

        iter.step_back(tok);
        block.push(parse_expression(iter)?);
    }
}

fn parse_struct_decl<'a>(iter: &mut TokenIter<'a>) -> Result<Type<'a>, CompileError> {
    let name = expect_ident(iter.next().unwrap())?;
    let next = iter.next().unwrap();
    match &next.item {
        Token::SemiColon => Ok(Type {
            name,
            kind: Kind::Unit,
        }),
        Token::OpenBlock(BlockDelim::Brace) => {
            let mut fields = Vec::new();
            loop {
                if try_consume_token(Token::CloseBlock(BlockDelim::Brace), iter) {
                    break;
                }

                let name = expect_ident(iter.next().unwrap())?;
                consume_token(Token::Colon, iter)?;
                let ty = parse_type(iter)?;
                fields.push(Field { name, ty });

                let tok = iter.next().unwrap();
                if tok.item == Token::CloseBlock(BlockDelim::Brace) {
                    break;
                } else if tok.item != Token::Comma {
                    CompileError::expected(
                        &[Token::Comma, Token::CloseBlock(BlockDelim::Brace)],
                        &tok,
                    )?;
                }
            }
            Ok(Type {
                name,
                kind: Kind::Struct(fields),
            })
        }
        _ => CompileError::expected(
            &[Token::SemiColon, Token::OpenBlock(BlockDelim::Brace)],
            &next,
        ),
    }
}

// parse a function, `fn` should already be consumed
fn parse_function<'a>(iter: &mut TokenIter<'a>) -> Result<Function<'a>, CompileError> {
    let mut func = Function::new(expect_ident(iter.next().unwrap())?);

    consume_token(Token::OpenBlock(BlockDelim::Parenthesis), iter)?;
    loop {
        let tok = iter.next().unwrap();
        if tok.item == Token::CloseBlock(BlockDelim::Parenthesis) {
            break;
        }

        let arg_name = expect_ident(tok)?;
        consume_token(Token::Colon, iter)?;
        func.add_argument(arg_name, parse_type(iter)?.map(|t| Some(t)))?;

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
        func.set_return(parse_type(iter)?);
        consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
    } else if tok.item != Token::OpenBlock(BlockDelim::Brace) {
        CompileError::expected(&Token::OpenBlock(BlockDelim::Brace), &tok)?;
    }

    func.set_body(parse_block(None, iter)?);

    Ok(func)
}
