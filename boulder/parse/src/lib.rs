use std::mem;

use diagnostics::{CompileError, Meta};

mod tokenize;

use tokenize::{Binop, BlockDelim, Keyword, Token, TokenIter};

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
type Pattern<'a> = hir::Pattern<'a, hir::UnresolvedIdentifiers<'a>>;
type Literal<'a> = hir::Literal<hir::UnresolvedIdentifiers<'a>>;

pub fn parse<'a>(src: &'a str) -> Result<Hir, CompileError> {
    let iter = &mut TokenIter::new(src);
    let mut hir = Hir::new();
    let mut attributes = Vec::new();
    while let Some(mut token) = iter.next() {
        match mem::replace(&mut token.item, Token::Invalid) {
            Token::Keyword(Keyword::Function) => {
                hir.add_function(parse_function(
                    mem::replace(&mut attributes, Vec::new()),
                    iter,
                )?)?;
            }
            Token::Keyword(Keyword::Struct) => {
                hir.add_type(parse_struct_decl(
                    mem::replace(&mut attributes, Vec::new()),
                    iter,
                )?)?;
            }
            Token::Attribute(value) => {
                attributes.push(token.replace(value));
            }
            Token::EOF => break,
            _ => CompileError::expected(
                &[
                    Token::Keyword(Keyword::Struct),
                    Token::Keyword(Keyword::Function),
                    Token::Attribute("".into()),
                    Token::EOF,
                ],
                &token,
            )?,
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

fn parse_pattern<'a>(iter: &mut TokenIter<'a>) -> Result<Pattern<'a>, CompileError> {
    let mut next = iter.next().unwrap();
    let name = match mem::replace(&mut next.item, Token::Invalid) {
        Token::Ident(v) => Some(next.replace(v)),
        Token::Underscore => None,
        _ => CompileError::expected(&[Token::Ident("".into()), Token::Underscore], &next)?,
    };
    consume_token(Token::Colon, iter)?;
    let ty = parse_type(iter)?;
    if let Some(name) = name {
        Ok(Pattern::Named(hir::UnresolvedVariable::New(
            name,
            ty.map(|t| Some(t)),
        )))
    } else {
        Ok(Pattern::Underscore(ty))
    }
}

fn check_expr_terminator<'a>(
    tok: Meta<'a, Token>,
    prev_checked: &[Token],
    expecting_open_brace: bool,
) -> Result<Meta<'a, Token>, CompileError> {
    match tok.item {
        Token::Comma | Token::SemiColon | Token::CloseBlock(_) => Ok(tok),
        Token::OpenBlock(BlockDelim::Brace) if expecting_open_brace => Ok(tok),
        _ => {
            let mut expected = vec![Token::SemiColon, Token::CloseBlock(BlockDelim::Brace)];
            if expecting_open_brace {
                expected.push(Token::OpenBlock(BlockDelim::Brace));
            }
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
    if let Token::Binop(Binop::BitOr) = &next.item {
        let mut parts = vec![first];
        let mut next = next;
        while let Token::Binop(Binop::BitOr) = &next.item {
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
    expecting_open_brace: bool,
) -> Result<Expression<'a>, CompileError> {
    let next = iter.next().unwrap();
    Ok(match next.item {
        Token::OpenBlock(BlockDelim::Parenthesis) => {
            let mut args = Vec::new();
            while !try_consume_token(Token::CloseBlock(BlockDelim::Parenthesis), iter) {
                args.push(parse_expression(iter, false)?);

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
        Token::OpenBlock(BlockDelim::Brace) => {
            if expecting_open_brace {
                iter.step_back(next);
                Expression::Variable((), hir::UnresolvedVariable::Existing(ident))
            } else {
                let mut fields = Vec::new();
                while !try_consume_token(Token::CloseBlock(BlockDelim::Brace), iter) {
                    let field_name = expect_ident(iter.next().unwrap())?;
                    consume_token(Token::Colon, iter)?;
                    let expr = parse_expression(iter, false)?;
                    fields.push((field_name, expr));

                    if !try_consume_token(Token::Comma, iter)
                        && !peek_token(Token::CloseBlock(BlockDelim::Brace), iter)
                    {
                        CompileError::expected(
                            &[Token::Comma, Token::CloseBlock(BlockDelim::Brace)],
                            &iter.next().unwrap(),
                        )?;
                    }
                }
                Expression::InitializeStruct((), ident, fields)
            }
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

/// the `match` is already consumed
fn parse_match<'a>(
    keyword: Meta<'a, ()>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    let value = parse_expression(iter, true)?;
    consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
    let mut match_arms = Vec::new();
    loop {
        if try_consume_token(Token::CloseBlock(BlockDelim::Brace), iter) {
            break;
        }

        let pattern = parse_pattern(iter)?;
        consume_token(Token::Arrow, iter)?;
        let expr = parse_expression(iter, false)?;

        match_arms.push(MatchArm { pattern, expr });

        if try_consume_token(Token::CloseBlock(BlockDelim::Brace), iter) {
            break;
        } else {
            consume_token(Token::Comma, iter)?
        }
    }

    Ok(Expression::Match((), keyword, Box::new(value), match_arms))
}

fn desugar_logical_ops<'a>(
    op: Binop,
    meta: Meta<'a, ()>,
    a: Expression<'a>,
    b: Expression<'a>,
) -> Expression<'a> {
    let cond = Expression::TypeRestriction(
        Box::new(a),
        meta.replace(hir::UnresolvedType::Named("Bool".into())),
    );

    let (a, b) = match op {
        Binop::And => (
            MatchArm {
                pattern: Pattern::Underscore(Meta::fake(hir::UnresolvedType::Named("True".into()))),
                expr: Expression::TypeRestriction(
                    Box::new(b),
                    meta.replace(hir::UnresolvedType::Named("Bool".into())),
                ),
            },
            MatchArm {
                pattern: Pattern::Underscore(Meta::fake(hir::UnresolvedType::Named(
                    "False".into(),
                ))),
                expr: Expression::Lit((), meta.replace(Literal::Unit("False".into()))),
            },
        ),
        Binop::Or => (
            MatchArm {
                pattern: Pattern::Underscore(Meta::fake(hir::UnresolvedType::Named("True".into()))),
                expr: Expression::Lit((), meta.replace(Literal::Unit("True".into()))),
            },
            MatchArm {
                pattern: Pattern::Underscore(Meta::fake(hir::UnresolvedType::Named(
                    "False".into(),
                ))),
                expr: Expression::TypeRestriction(
                    Box::new(b),
                    meta.replace(hir::UnresolvedType::Named("Bool".into())),
                ),
            },
        ),
        _ => unreachable!("invalid logical operation: {:?}", op),
    };

    Expression::TypeRestriction(
        Box::new(Expression::Match(
            (),
            meta.clone(),
            Box::new(cond),
            vec![a, b],
        )),
        meta.replace(hir::UnresolvedType::Named("Bool".into())),
    )
}

/// the `if` is already consumed
fn parse_if<'a>(
    if_meta: Meta<'a, ()>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    let expr = parse_expression(iter, true)?;
    let expr = Expression::TypeRestriction(
        Box::new(expr),
        if_meta.replace(hir::UnresolvedType::Named("Bool".into())),
    );
    consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
    let a = MatchArm {
        pattern: Pattern::Underscore(Meta::fake(hir::UnresolvedType::Named("True".into()))),
        expr: parse_block(None, iter)?,
    };
    let b = MatchArm {
        pattern: Pattern::Underscore(Meta::fake(hir::UnresolvedType::Named("False".into()))),
        expr: if try_consume_token(Token::Keyword(Keyword::Else), iter) {
            consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
            parse_block(None, iter)?
        } else {
            Expression::Lit((), if_meta.replace(Literal::Unit("Empty".into())))
        },
    };

    Ok(Expression::Match((), if_meta, Box::new(expr), vec![a, b]))
}

fn parse_variable_decl<'a>(
    iter: &mut TokenIter<'a>,
    expecting_open_brace: bool,
) -> Result<Expression<'a>, CompileError> {
    let name = expect_ident(iter.next().unwrap())?;

    let ty = if try_consume_token(Token::Colon, iter) {
        Some(parse_type(iter)?)
    } else {
        None
    }
    .map_or_else(|| name.simplify().replace(None), |v| v.map(|t| Some(t)));

    if try_consume_token(Token::Assignment, iter) {
        let input = parse_expression(iter, expecting_open_brace)?;
        Ok(Expression::Assignment(
            (),
            hir::UnresolvedVariable::New(name, ty),
            Box::new(input),
        ))
    } else {
        let tok = iter.next().unwrap();
        iter.step_back(check_expr_terminator(
            tok,
            &[Token::Assignment],
            expecting_open_brace,
        )?);
        Ok(Expression::Variable(
            (),
            hir::UnresolvedVariable::New(name, ty),
        ))
    }
}

fn parse_scope<'a>(
    scope: Meta<'a, Box<str>>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    consume_token(Token::Colon, iter)?;
    let next = iter.next().unwrap();
    match next.item {
        Token::OpenBlock(BlockDelim::Brace) => parse_block(Some(scope), iter),
        Token::Keyword(Keyword::Loop) => {
            consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
            if let Expression::Block((), scope, body) = parse_block(Some(scope), iter)? {
                Ok(Expression::Loop((), scope, body))
            } else {
                unreachable!("parse_block returned an unexpected expression")
            }
        }
        Token::Keyword(Keyword::While) => parse_while(Some(scope), next.simplify(), iter),
        _ => CompileError::expected(
            &[
                Token::OpenBlock(BlockDelim::Brace),
                Token::Keyword(Keyword::Loop),
                Token::Keyword(Keyword::While),
            ],
            &next,
        ),
    }
}

fn parse_while<'a>(
    name: Option<Meta<'a, Box<str>>>,
    meta: Meta<'a, ()>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    let expr = parse_expression(iter, true)?;
    consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
    let stay = MatchArm {
        pattern: Pattern::Underscore(Meta::fake(hir::UnresolvedType::Named("True".into()))),
        expr: Expression::Lit((), meta.replace(Literal::Unit("Empty".into()))),
    };

    let empty_lit = Expression::Lit((), meta.replace(Literal::Unit("Empty".into())));
    let exit = MatchArm {
        pattern: Pattern::Underscore(Meta::fake(hir::UnresolvedType::Named("False".into()))),
        expr: Expression::Break((), meta.replace(None), Box::new(empty_lit)),
    };

    if let Expression::Block((), scope, mut body) = parse_block(name, iter)? {
        let check = Expression::TypeRestriction(
            Box::new(Expression::Match(
                (),
                meta.simplify(),
                Box::new(expr),
                vec![stay, exit],
            )),
            meta.replace(hir::UnresolvedType::Sum(vec![
                meta.replace("Empty".into()),
                meta.replace("Never".into()),
            ])),
        );
        body.insert(0, Expression::Statement((), Box::new(check)));
        Ok(Expression::TypeRestriction(
            Box::new(Expression::Loop((), scope, body)),
            meta.replace(hir::UnresolvedType::Named("Empty".into())),
        ))
    } else {
        unreachable!("parse_block returned an unexpected expression")
    }
}

/// parse the rhs of a binary operation, each binop must have a priority greater than `priority`.
fn parse_binop_rhs<'a>(
    priority: u32,
    iter: &mut TokenIter<'a>,
    expecting_open_brace: bool,
) -> Result<Expression<'a>, CompileError> {
    let mut start = iter.next().unwrap();
    let mut expr = match mem::replace(&mut start.item, Token::Invalid) {
        Token::Scope(v) => parse_scope(start.replace(v), iter)?,
        Token::Invert => {
            let expr = parse_binop_rhs(std::u32::MAX, iter, expecting_open_brace)?;
            Expression::UnaryOperation(
                (),
                start.replace(hir::UnaryOperation::Invert),
                Box::new(expr),
            )
        }
        Token::Ident(v) => parse_ident_expr(start.replace(v), iter, expecting_open_brace)?,
        Token::Integer(c) => Expression::Lit((), start.replace(Literal::Integer(c))),
        Token::Keyword(Keyword::Match) => parse_match(start.simplify(), iter)?,
        Token::Keyword(Keyword::If) => parse_if(start.simplify(), iter)?,
        Token::OpenBlock(BlockDelim::Parenthesis) => {
            let expr = parse_expression(iter, false)?;
            consume_token(Token::CloseBlock(BlockDelim::Parenthesis), iter)?;
            expr
        }
        Token::OpenBlock(BlockDelim::Brace) => parse_block(None, iter)?,
        Token::Keyword(Keyword::Loop) => {
            consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
            if let Expression::Block((), scope, body) = parse_block(None, iter)? {
                Expression::Loop((), scope, body)
            } else {
                unreachable!("parse_block returned an unexpected expression")
            }
        }
        Token::Keyword(Keyword::While) => parse_while(None, start.simplify(), iter)?,
        _ => CompileError::expected(
            &[
                Token::Scope("".into()),
                Token::Ident("".into()),
                Token::Integer(0),
                Token::Keyword(Keyword::Match),
                Token::Keyword(Keyword::If),
                Token::Keyword(Keyword::Loop),
                Token::Keyword(Keyword::While),
                Token::OpenBlock(BlockDelim::Parenthesis),
            ],
            &start,
        )?,
    };

    let next = iter.next().unwrap();
    Ok(match &next.item {
        Token::Binop(_) => {
            let mut next = next;
            while let Token::Binop(op) = next.item {
                if op.priority() > priority {
                    expr = op.as_hir_expr(
                        next,
                        expr,
                        parse_binop_rhs(op.priority(), iter, expecting_open_brace)?,
                    );
                } else {
                    break;
                }
                next = iter.next().unwrap();
            }
            iter.step_back(next);
            expr
        }
        Token::Dot => {
            let mut next = next;
            while let Token::Dot = next.item {
                expr = Expression::FieldAccess(
                    (),
                    Box::new(expr),
                    expect_ident(iter.next().unwrap())?,
                );
                next = iter.next().unwrap();
            }

            while let Token::Binop(op) = next.item {
                if op.priority() > priority {
                    expr = op.as_hir_expr(
                        next,
                        expr,
                        parse_binop_rhs(op.priority(), iter, expecting_open_brace)?,
                    );
                } else {
                    break;
                }
                next = iter.next().unwrap();
            }
            iter.step_back(next);

            expr
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
    expecting_open_brace: bool,
) -> Result<Expression<'a>, CompileError> {
    let mut next = iter.next().unwrap();
    loop {
        match next.item {
            Token::Binop(op) => {
                lhs = op.as_hir_expr(
                    next.simplify(),
                    lhs,
                    parse_binop_rhs(op.priority(), iter, expecting_open_brace)?,
                );
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
                &[Token::Binop(Binop::Add), Token::Dot, Token::Colon],
                expecting_open_brace,
            )?);
            Ok(lhs)
        }
    }
}

fn parse_expression<'a>(
    iter: &mut TokenIter<'a>,
    expecting_open_brace: bool,
) -> Result<Expression<'a>, CompileError> {
    let mut start = iter.next().unwrap();
    match mem::replace(&mut start.item, Token::Invalid) {
        Token::OpenBlock(BlockDelim::Brace) => {
            let block = parse_block(None, iter)?;
            parse_binop(block, iter, expecting_open_brace)
        }
        Token::Keyword(Keyword::Loop) => {
            consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
            if let Expression::Block((), scope, body) = parse_block(None, iter)? {
                parse_binop(
                    Expression::Loop((), scope, body),
                    iter,
                    expecting_open_brace,
                )
            } else {
                unreachable!("parse_block returned an unexpected expression")
            }
        }
        Token::Keyword(Keyword::While) => {
            let expr = parse_while(None, start.simplify(), iter)?;
            parse_binop(expr, iter, expecting_open_brace)
        }
        Token::Scope(v) => parse_binop(
            parse_scope(start.replace(v), iter)?,
            iter,
            expecting_open_brace,
        ),
        Token::OpenBlock(BlockDelim::Parenthesis) => {
            let expr = parse_expression(iter, false)?;
            consume_token(Token::CloseBlock(BlockDelim::Parenthesis), iter)?;
            parse_binop(expr, iter, expecting_open_brace)
        }
        Token::Keyword(Keyword::Let) => parse_variable_decl(iter, expecting_open_brace),
        Token::Keyword(Keyword::Match) => {
            let expr = parse_match(start.simplify(), iter)?;
            parse_binop(expr, iter, expecting_open_brace)
        }
        Token::Keyword(Keyword::If) => {
            let expr = parse_if(start.simplify(), iter)?;
            parse_binop(expr, iter, expecting_open_brace)
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
                Box::new(parse_expression(iter, expecting_open_brace)?),
            ))
        }
        Token::Keyword(Keyword::Return) => Ok(Expression::Break(
            (),
            start.replace(Some("fn".into())),
            Box::new(parse_expression(iter, expecting_open_brace)?),
        )),
        Token::Invert => {
            let expr = parse_binop_rhs(std::u32::MAX, iter, expecting_open_brace)?;
            parse_binop(
                Expression::UnaryOperation(
                    (),
                    start.replace(hir::UnaryOperation::Invert),
                    Box::new(expr),
                ),
                iter,
                expecting_open_brace,
            )
        }
        Token::Ident(v) => {
            let expr = parse_ident_expr(start.replace(v), iter, expecting_open_brace)?;
            let next = iter.next().unwrap();
            Ok(match &next.item {
                Token::Assignment => {
                    if let Expression::Variable((), var) = expr {
                        Expression::Assignment(
                            (),
                            var,
                            Box::new(parse_expression(iter, expecting_open_brace)?),
                        )
                    } else {
                        Err(check_expr_terminator(
                            next,
                            &[Token::Assignment],
                            expecting_open_brace,
                        )
                        .unwrap_err())?
                    }
                }
                _ => {
                    iter.step_back(next);
                    parse_binop(expr, iter, expecting_open_brace)?
                }
            })
        }
        Token::Integer(c) => {
            let expr = Expression::Lit((), start.replace(Literal::Integer(c)));
            parse_binop(expr, iter, expecting_open_brace)
        }
        _ => CompileError::expected(
            &[
                Token::OpenBlock(BlockDelim::Brace),
                Token::Keyword(Keyword::Loop),
                Token::Scope("".into()),
                Token::OpenBlock(BlockDelim::Parenthesis),
                Token::Keyword(Keyword::Let),
                Token::Keyword(Keyword::Match),
                Token::Keyword(Keyword::If),
                Token::Keyword(Keyword::Break),
                Token::Keyword(Keyword::Return),
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
                            .replace(None)
                            .extend_left('{')
                    },
                    |v| v.map(|v| Some(v)),
                ),
                block,
            ));
        }

        iter.step_back(tok);
        block.push(parse_expression(iter, false)?);
    }
}

fn parse_struct_decl<'a>(
    attributes: Vec<Meta<'a, Box<str>>>,
    iter: &mut TokenIter<'a>,
) -> Result<Type<'a>, CompileError> {
    let name = expect_ident(iter.next().unwrap())?;
    let next = iter.next().unwrap();
    match &next.item {
        Token::SemiColon => Ok(Type {
            name,
            attributes,
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
                attributes,
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
fn parse_function<'a>(
    attributes: Vec<Meta<'a, Box<str>>>,
    iter: &mut TokenIter<'a>,
) -> Result<Function<'a>, CompileError> {
    let mut func = Function::new(expect_ident(iter.next().unwrap())?);
    func.attributes = attributes;

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
