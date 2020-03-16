#![allow(clippy::match_ref_pats)]

#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use std::{fs::File, io::Read, path::PathBuf};

use tindex::TVec;

use global_ctx::GlobalCtx;

use diagnostics::{CompileError, Meta, Span};

use hir::attr::{FunctionAttribute, ModuleAttribute, TypeAttribute};

mod tokenize;

use tokenize::{Binop, BlockDelim, Keyword, Token, TokenIter};

type Expression<'a> = hir::expr::Expression<
    'a,
    hir::traits::UnresolvedIdentifiers<'a>,
    hir::traits::UnresolvedTypes<'a>,
>;
type Function<'a> = hir::func::Function<
    'a,
    hir::traits::UnresolvedIdentifiers<'a>,
    hir::traits::UnresolvedTypes<'a>,
    Option<hir::UnresolvedType<'a>>,
>;
type Hir<'a> = hir::Hir<
    'a,
    hir::traits::UnresolvedIdentifiers<'a>,
    hir::traits::UnresolvedTypes<'a>,
    Option<hir::UnresolvedType<'a>>,
    hir::UnresolvedType<'a>,
>;
type Type<'a> = hir::ty::Type<'a, hir::UnresolvedType<'a>>;
type Kind<'a> = hir::ty::Kind<'a, hir::UnresolvedType<'a>>;
type Field<'a> = hir::ty::Field<'a, hir::UnresolvedType<'a>>;
type MatchArm<'a> = hir::expr::MatchArm<
    'a,
    hir::traits::UnresolvedIdentifiers<'a>,
    hir::traits::UnresolvedTypes<'a>,
>;
type Pattern<'a> = hir::Pattern<'a, hir::traits::UnresolvedIdentifiers<'a>>;
type Literal<'a> = hir::Literal<hir::traits::UnresolvedIdentifiers<'a>>;

pub fn parse<'a>(ctx: &'a GlobalCtx, src: &'a str, file: &'a str) -> Result<Hir<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse");
    let iter = &mut TokenIter::new(src, file);
    let mut hir = Hir::new();
    parse_module(ctx, &mut hir, &mut Vec::new(), iter)?;

    // TODO: allow for no_std
    let std = include_str!("../../_std/lib.bo");
    hir.add_module(&[], Meta::fake("std")).unwrap();
    parse_module(
        ctx,
        &mut hir,
        &mut vec!["std"],
        &mut TokenIter::new(std, "./_std/lib.bo"),
    )?;
    consume_token(Token::EOF, iter)?;
    Ok(hir)
}

pub fn parse_module<'a>(
    ctx: &'a GlobalCtx,
    hir: &mut Hir<'a>,
    at: &mut Vec<&'a str>,
    iter: &mut TokenIter<'a>,
) -> Result<(), CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_module");
    let mut attributes = Vec::new();
    let mut imports = Vec::new();
    while let Some(token) = iter.next() {
        match token.item {
            Token::Keyword(Keyword::Module) => {
                parse_module_decl(
                    ctx,
                    hir,
                    token,
                    at,
                    attributes
                        .into_iter()
                        .map(|(name, args)| ModuleAttribute::new(name, args))
                        .collect::<Result<_, _>>()?,
                    iter,
                )?;
                attributes = Vec::new();
            }
            Token::Keyword(Keyword::Use) => {
                if let Some((name, _)) = attributes.first() {
                    return CompileError::new(
                        &name,
                        format_args!("Cannot apply attribute `{}` to imports", name.item),
                    );
                }

                imports.push(parse_import(at, iter)?);
            }
            Token::Keyword(Keyword::Function) => {
                let func = parse_function(
                    at,
                    attributes
                        .into_iter()
                        .map(|(name, args)| FunctionAttribute::new(name, args))
                        .collect::<Result<_, _>>()?,
                    iter,
                )?;
                hir.add_function(&at, func)?;
                attributes = Vec::new();
            }
            Token::Keyword(Keyword::Struct) => {
                let ty = parse_struct_decl(
                    at,
                    attributes
                        .into_iter()
                        .map(|(name, args)| TypeAttribute::new(name, args))
                        .collect::<Result<_, _>>()?,
                    iter,
                )?;
                hir.add_type(&at, ty)?;
                attributes = Vec::new();
            }
            Token::Keyword(Keyword::Union) => {
                let ty = parse_union_decl(
                    at,
                    attributes
                        .into_iter()
                        .map(|(name, args)| TypeAttribute::new(name, args))
                        .collect::<Result<_, _>>()?,
                    iter,
                )?;
                hir.add_type(&at, ty)?;
                attributes = Vec::new();
            }
            Token::Attribute(value) => {
                attributes.push(parse_attribute(token.replace(value), iter)?);
            }
            Token::EOF | Token::CloseBlock(BlockDelim::Brace) => {
                iter.step_back(token);
                break;
            }
            _ => CompileError::expected(
                &[
                    Token::Keyword(Keyword::Struct),
                    Token::Keyword(Keyword::Union),
                    Token::Keyword(Keyword::Function),
                    Token::Attribute(""),
                    Token::EOF,
                ],
                &token,
            )?,
        }
    }

    for import in imports {
        hir.add_import(at, &import)?;
    }
    Ok(())
}

fn peek_token(expected: Token, iter: &mut TokenIter) -> bool {
    let tok = iter.next_token();
    if expected == tok.item {
        iter.step_back(tok);
        true
    } else {
        iter.step_back(tok);
        false
    }
}

fn try_consume_token(expected: Token, iter: &mut TokenIter) -> bool {
    let tok = iter.next_token();
    if expected == tok.item {
        true
    } else {
        iter.step_back(tok);
        false
    }
}

fn consume_token(expected: Token, iter: &mut TokenIter) -> Result<(), CompileError> {
    let tok = iter.next_token();
    if expected == tok.item {
        Ok(())
    } else {
        CompileError::expected(&expected, &tok)
    }
}

fn expect_ident<'a>(tok: Meta<'a, Token<'a>>) -> Result<Meta<'a, &'a str>, CompileError> {
    let tok = tok.map(|t| {
        if let Token::Ident(value) = t {
            value
        } else {
            ""
        }
    });
    if tok.item.is_empty() {
        CompileError::expected(&"Ident", &tok)
    } else {
        Ok(tok)
    }
}

fn parse_module_decl<'a>(
    ctx: &'a GlobalCtx,
    hir: &mut Hir<'a>,
    mod_tok: Meta<'a, Token<'a>>,
    at: &mut Vec<&'a str>,
    attributes: Vec<Meta<'a, ModuleAttribute<'a>>>,
    iter: &mut TokenIter<'a>,
) -> Result<(), CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_module_decl");
    let module_name = expect_ident(iter.next_token())?;
    let name = module_name.item;
    hir.add_module(&at, module_name.map(Into::into))?;
    at.push(name);
    if try_consume_token(Token::OpenBlock(BlockDelim::Brace), iter) {
        for attr in attributes {
            match &attr.item {
                &ModuleAttribute::Path(_) => {
                    CompileError::new(&attr, "Invalid `path` attribute for inline module")?;
                }
                &ModuleAttribute::Str(_) => unreachable!(),
            }
        }

        parse_module(ctx, hir, at, iter)?;
        consume_token(Token::CloseBlock(BlockDelim::Brace), iter)?;
        at.pop();
        Ok(())
    } else if try_consume_token(Token::SemiColon, iter) {
        let mut path = None;
        for attr in attributes {
            match &attr.item {
                &ModuleAttribute::Path(s) => {
                    if let Some(old) = path {
                        return CompileError::build(
                            &old,
                            "Attribute `path` used more than once on module",
                        )
                        .with_location(&attr)
                        .build();
                    } else {
                        path = Some(attr.replace(s));
                    }
                }
                &ModuleAttribute::Str(_) => unreachable!(),
            }
        }

        if let Some(p) = path {
            let mut path_buf = PathBuf::from(iter.file());
            path_buf.set_file_name(p.item);
            path_buf.set_extension("bo");
            if let Ok(mut file) = File::open(&path_buf) {
                let mut src = String::new();
                if file.read_to_string(&mut src).is_ok() {
                    // TODO: add string storage
                    let s = ctx.insert_str(src.into_boxed_str());
                    let f =
                        ctx.insert_str(path_buf.to_string_lossy().into_owned().into_boxed_str());
                    let iter = &mut TokenIter::new(s, f);
                    parse_module(ctx, hir, at, iter)?;
                    consume_token(Token::EOF, iter)?;
                    at.pop();
                    return Ok(());
                }
            }

            CompileError::new(
                &p,
                format_args!("Unable to open file `{}`", path_buf.display()),
            )
        } else {
            CompileError::build(&mod_tok, "Missing `path` attribute on extern module")
                .with_help("Consider adding `@path(<file>)`")
                .build()?
        }
    } else {
        CompileError::expected(
            &[Token::OpenBlock(BlockDelim::Brace), Token::SemiColon],
            &iter.next_token(),
        )
    }
}

fn parse_attribute<'a>(
    name: Meta<'a, &'a str>,
    iter: &mut TokenIter<'a>,
) -> Result<(Meta<'a, &'a str>, Vec<Meta<'a, &'a str>>), CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_attribute");
    let mut args = Vec::new();
    if try_consume_token(Token::OpenBlock(BlockDelim::Parenthesis), iter) {
        while !try_consume_token(Token::CloseBlock(BlockDelim::Parenthesis), iter) {
            args.push(expect_ident(iter.next_token())?);

            if !try_consume_token(Token::Comma, iter)
                && !peek_token(Token::CloseBlock(BlockDelim::Parenthesis), iter)
            {
                CompileError::expected(
                    &[Token::Comma, Token::CloseBlock(BlockDelim::Parenthesis)],
                    &iter.next_token(),
                )?;
            }
        }
    }

    Ok((name, args))
}

fn parse_pattern<'a>(iter: &mut TokenIter<'a>) -> Result<Pattern<'a>, CompileError> {
    let next = iter.next_token();
    let name = match next.item {
        Token::Ident(v) => Some(next.replace(v)),
        Token::Underscore => None,
        _ => CompileError::expected(&[Token::Ident(""), Token::Underscore], &next)?,
    };
    consume_token(Token::Colon, iter)?;
    let ty = parse_type(iter)?;
    if let Some(name) = name {
        Ok(Pattern::Named(hir::UnresolvedVariable::New(
            name.map(Into::into),
            ty.map(Some),
        )))
    } else {
        Ok(Pattern::Underscore(ty))
    }
}

fn check_expr_terminator<'a>(
    tok: Meta<'a, Token<'a>>,
    prev_checked: &[Token<'a>],
    expecting_open_brace: bool,
) -> Result<Meta<'a, Token<'a>>, CompileError> {
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
    #[cfg(feature = "profiler")]
    profile_scope!("parse_type");
    let first = expect_ident(iter.next_token())?.map(Into::into);
    let next = iter.next_token();
    if let Token::Binop(Binop::BitOr) = &next.item {
        let mut parts: Vec<Meta<'a, Box<str>>> = vec![first];
        let mut next = next;
        while let Token::Binop(Binop::BitOr) = &next.item {
            let component = expect_ident(iter.next_token())?.map(Into::into);
            parts.push(component);
            next = iter.next_token();
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
        Ok(first.map(hir::UnresolvedType::Named))
    }
}

fn parse_ident_expr<'a>(
    ident: Meta<'a, &'a str>,
    iter: &mut TokenIter<'a>,
    expecting_open_brace: bool,
) -> Result<Expression<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_ident_expr");
    let next = iter.next_token();
    Ok(match next.item {
        Token::OpenBlock(BlockDelim::Parenthesis) => {
            let mut args = Vec::new();
            while !try_consume_token(Token::CloseBlock(BlockDelim::Parenthesis), iter) {
                args.push(parse_expr(iter, false, None)?);

                if !try_consume_token(Token::Comma, iter)
                    && !peek_token(Token::CloseBlock(BlockDelim::Parenthesis), iter)
                {
                    CompileError::expected(
                        &[Token::Comma, Token::CloseBlock(BlockDelim::Parenthesis)],
                        &iter.next_token(),
                    )?;
                }
            }

            match ident.item {
                "to_bytes" | "from_bytes" | "dbg" => {
                    if args.len() == 1 {
                        let op = ident.map(|i| match i {
                            "to_bytes" => hir::expr::UnaryOperation::ToBytes,
                            "from_bytes" => hir::expr::UnaryOperation::FromBytes,
                            "dbg" => hir::expr::UnaryOperation::Debug,
                            _ => unreachable!(),
                        });

                        Expression::UnaryOperation((), op, Box::new(args.pop().unwrap()))
                    } else {
                        let location = args
                            .last()
                            .map_or(ident.simplify(), |arg| ident.simplify().append(arg.span()))
                            .extend_right(')');

                        CompileError::build(
                            &ident,
                            format_args!(
                                "This function takes 1 parameter but received {}",
                                args.len()
                            ),
                        )
                        .with_location(&location)
                        .build()?
                    }
                }
                _ => Expression::FunctionCall((), ident.map(Into::into), args),
            }
        }
        Token::OpenBlock(BlockDelim::Brace) => {
            if expecting_open_brace {
                iter.step_back(next);
                Expression::Variable((), hir::UnresolvedVariable::Existing(ident.map(Into::into)))
            } else {
                let mut fields = Vec::new();
                while !try_consume_token(Token::CloseBlock(BlockDelim::Brace), iter) {
                    let field_name = expect_ident(iter.next_token())?.map(Into::into);
                    consume_token(Token::Colon, iter)?;
                    let expr = parse_expr(iter, false, None)?;
                    fields.push((field_name, expr));

                    if !try_consume_token(Token::Comma, iter)
                        && !peek_token(Token::CloseBlock(BlockDelim::Brace), iter)
                    {
                        CompileError::expected(
                            &[Token::Comma, Token::CloseBlock(BlockDelim::Brace)],
                            &iter.next_token(),
                        )?;
                    }
                }
                Expression::InitializeStruct((), ident.map(Into::into), fields)
            }
        }
        Token::Colon => {
            let var =
                Expression::Variable((), hir::UnresolvedVariable::Existing(ident.map(Into::into)));
            Expression::TypeRestriction(Box::new(var), parse_type(iter)?)
        }
        _ => {
            iter.step_back(next);
            Expression::Variable((), hir::UnresolvedVariable::Existing(ident.map(Into::into)))
        }
    })
}

/// the `match` is already consumed
///
/// ```plain
/// match expr {
/// #     ^
///     binding: pat -> expr,
///     binding: pat -> expr,
/// }
/// ```
fn parse_match<'a>(
    keyword: Meta<'a, ()>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_match");
    let value = parse_expr(iter, true, None)?;
    consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
    let mut match_arms = Vec::new();
    loop {
        if try_consume_token(Token::CloseBlock(BlockDelim::Brace), iter) {
            break;
        }

        let pattern = parse_pattern(iter)?;
        consume_token(Token::Arrow, iter)?;
        let expr = parse_expr(iter, false, None)?;

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
    #[cfg(feature = "profiler")]
    profile_scope!("desugar_logical_ops");
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
///
/// # Examples
///
/// ```plain
/// if cond {
/// #  ^
///     expr
/// } else {
///     expr
/// }
/// ```
fn parse_if<'a>(
    if_meta: Meta<'a, ()>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("desugar_if");
    let expr = parse_expr(iter, true, None)?;
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
    #[cfg(feature = "profiler")]
    profile_scope!("parse_variable_decl");
    let name = expect_ident(iter.next_token())?;

    let ty = if try_consume_token(Token::Colon, iter) {
        Some(parse_type(iter)?)
    } else {
        None
    }
    .map_or_else(|| name.simplify().replace(None), |v| v.map(Some));

    if try_consume_token(Token::Assignment, iter) {
        let input = parse_expr(iter, expecting_open_brace, None)?;
        Ok(Expression::Assignment(
            (),
            hir::UnresolvedVariable::New(name, ty),
            Box::new(input),
        ))
    } else {
        let tok = iter.next_token();
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

/// Examples
///
/// ```plain
/// 'scope: { expr; expr }
/// #       ^
///
/// 'scope: loop { expr; expr }
/// #       ^
///
/// 'scope: while { expr; expr }
/// #       ^
/// ```
fn parse_scope<'a>(
    scope: Meta<'a, &'a str>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_scope");
    consume_token(Token::Colon, iter)?;
    let next = iter.next_token();
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

/// # Examples
///
/// ```plain
fn parse_while<'a>(
    name: Option<Meta<'a, &'a str>>,
    meta: Meta<'a, ()>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_while");
    let expr = parse_expr(iter, true, None)?;
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

/// parse an expr which may be the rhs of a binary operation,
/// in which case each binop must have a priority greater than `priority`.
fn parse_expr<'a>(
    iter: &mut TokenIter<'a>,
    expecting_open_brace: bool,
    priority: Option<u32>,
) -> Result<Expression<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_expr");
    let start = iter.next_token();
    let mut expr = match start.item {
        Token::Scope(v) => parse_scope(start.replace(v), iter)?,
        Token::Invert => {
            let expr = parse_expr(iter, expecting_open_brace, Some(std::u32::MAX))?;
            Expression::UnaryOperation(
                (),
                start.replace(hir::expr::UnaryOperation::Invert),
                Box::new(expr),
            )
        }
        Token::Ident(v) => parse_ident_expr(start.replace(v), iter, expecting_open_brace)?,
        Token::Integer(c) => Expression::Lit((), start.replace(Literal::Integer(c))),
        Token::Keyword(Keyword::Let) => {
            if let Some(_priority) = priority {
                CompileError::new(&start, "`let` binding may not be using in a binop")
            } else {
                parse_variable_decl(iter, expecting_open_brace)
            }?
        }
        Token::Keyword(Keyword::Match) => parse_match(start.simplify(), iter)?,
        Token::Keyword(Keyword::If) => parse_if(start.simplify(), iter)?,
        Token::Keyword(Keyword::Loop) => {
            consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
            if let Expression::Block((), scope, body) = parse_block(None, iter)? {
                Expression::Loop((), scope, body)
            } else {
                unreachable!("parse_block returned an unexpected expression")
            }
        }
        Token::Keyword(Keyword::While) => parse_while(None, start.simplify(), iter)?,
        Token::Keyword(Keyword::Break) => {
            let next = iter.next_token();
            let scope = if let Token::Scope(v) = next.item {
                next.replace(Some(v))
            } else {
                iter.step_back(next);
                start.replace(None)
            };

            Expression::Break(
                (),
                scope.map(|v| v.map(Into::into)),
                Box::new(parse_expr(iter, expecting_open_brace, None)?),
            )
        }
        Token::Keyword(Keyword::Return) => Expression::Break(
            (),
            start.replace(Some("fn")),
            Box::new(parse_expr(iter, expecting_open_brace, None)?),
        ),
        Token::OpenBlock(BlockDelim::Parenthesis) => {
            let expr = parse_expr(iter, false, None)?;
            consume_token(Token::CloseBlock(BlockDelim::Parenthesis), iter)?;
            expr
        }
        Token::OpenBlock(BlockDelim::Brace) => parse_block(None, iter)?,
        _ => CompileError::expected(
            &[
                Token::Scope(""),
                Token::Ident(""),
                Token::Integer(0),
                Token::Keyword(Keyword::Let),
                Token::Keyword(Keyword::Match),
                Token::Keyword(Keyword::If),
                Token::Keyword(Keyword::Loop),
                Token::Keyword(Keyword::While),
                Token::Keyword(Keyword::Break),
                Token::Keyword(Keyword::Return),
                Token::OpenBlock(BlockDelim::Parenthesis),
                Token::OpenBlock(BlockDelim::Brace),
            ],
            &start,
        )?,
    };

    loop {
        let next = iter.next_token();
        match next.item {
            Token::Binop(op) => {
                if priority.map_or(true, |p| p < op.priority()) {
                    expr = op.as_hir_expr(
                        next,
                        expr,
                        parse_expr(iter, expecting_open_brace, Some(op.priority()))?,
                    );
                } else {
                    iter.step_back(next);
                    return Ok(expr);
                }
            }
            Token::Dot => {
                expr = Expression::FieldAccess(
                    (),
                    Box::new(expr),
                    expect_ident(iter.next_token())?.map(Into::into),
                );
            }
            Token::Colon => expr = Expression::TypeRestriction(Box::new(expr), parse_type(iter)?),
            Token::Assignment => {
                let err = |meta| {
                    CompileError::build(meta, "Invalid left-hand side of assignment")
                        .with_help("try comparing for equality using `==`")
                        .build()
                };

                match expr {
                    Expression::Variable((), var) => {
                        return Ok(Expression::Assignment(
                            (),
                            var,
                            Box::new(parse_expr(iter, expecting_open_brace, None)?),
                        ));
                    }
                    Expression::FieldAccess((), expr, _field) => {
                        if let Expression::Variable((), _var) = *expr {
                            unimplemented!("overwriting fields")
                        } else {
                            return err(&expr.span());
                        }
                    }
                    expr => return err(&expr.span()),
                }
            }
            _ => {
                iter.step_back(check_expr_terminator(
                    next,
                    &[Token::Binop(Binop::Add), Token::Dot, Token::Colon],
                    expecting_open_brace,
                )?);
                return Ok(expr);
            }
        }
    }
}

fn parse_block<'a>(
    scope: Option<Meta<'a, &'a str>>,
    iter: &mut TokenIter<'a>,
) -> Result<Expression<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_block");
    let mut block = Vec::new();
    let start = iter.current_offset();
    let line = iter.current_line();
    loop {
        let mut tok = iter.next_token();

        while tok.item == Token::SemiColon {
            tok = iter.next_token();
            if let Some(expr) = block.pop() {
                block.push(Expression::Statement((), Box::new(expr)));
            }
        }

        if tok.item == Token::CloseBlock(BlockDelim::Brace) {
            let end = iter.current_offset();
            return Ok(Expression::Block(
                (),
                scope.map_or_else(
                    || {
                        Meta::empty(iter.source(), iter.file(), line, start..end)
                            .replace(None)
                            .extend_left('{')
                    },
                    |v| v.map(Some),
                ),
                block,
            ));
        }

        iter.step_back(tok);
        block.push(parse_expr(iter, false, None)?);
    }
}

fn parse_union_decl<'a>(
    at: &mut Vec<&'a str>,
    attributes: Vec<Meta<'a, TypeAttribute<'a>>>,
    iter: &mut TokenIter<'a>,
) -> Result<Type<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_union_decl");
    let name = expect_ident(iter.next_token())?.map(Into::into);
    consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
    let mut fields = TVec::new();
    loop {
        if try_consume_token(Token::CloseBlock(BlockDelim::Brace), iter) {
            break;
        }

        let name = expect_ident(iter.next_token())?.map(Into::into);
        consume_token(Token::Colon, iter)?;
        let ty = parse_type(iter)?;
        fields.push(Field { name, ty });

        let tok = iter.next_token();
        if tok.item == Token::CloseBlock(BlockDelim::Brace) {
            break;
        } else if tok.item != Token::Comma {
            CompileError::expected(&[Token::Comma, Token::CloseBlock(BlockDelim::Brace)], &tok)?;
        }
    }
    Ok(Type {
        at: at.clone(),
        name,
        attributes,
        kind: Kind::Union(fields),
    })
}

fn parse_struct_decl<'a>(
    at: &mut Vec<&'a str>,
    attributes: Vec<Meta<'a, TypeAttribute<'a>>>,
    iter: &mut TokenIter<'a>,
) -> Result<Type<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_struct_decl");
    let name = expect_ident(iter.next_token())?.map(Into::into);
    let next = iter.next_token();
    match &next.item {
        Token::SemiColon => Ok(Type {
            at: at.clone(),
            name,
            attributes,
            kind: Kind::Unit,
        }),
        Token::OpenBlock(BlockDelim::Brace) => {
            let mut fields = TVec::new();
            loop {
                if try_consume_token(Token::CloseBlock(BlockDelim::Brace), iter) {
                    break;
                }

                let name = expect_ident(iter.next_token())?.map(Into::into);
                consume_token(Token::Colon, iter)?;
                let ty = parse_type(iter)?;
                fields.push(Field { name, ty });

                let tok = iter.next_token();
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
                at: at.clone(),
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
    at: &mut Vec<&'a str>,
    attributes: Vec<Meta<'a, FunctionAttribute<'a>>>,
    iter: &mut TokenIter<'a>,
) -> Result<Function<'a>, CompileError> {
    #[cfg(feature = "profiler")]
    profile_scope!("parse_function");
    let mut func = Function::new(expect_ident(iter.next_token())?.map(Into::into), at.clone());
    func.attributes = attributes;

    consume_token(Token::OpenBlock(BlockDelim::Parenthesis), iter)?;
    loop {
        let tok = iter.next_token();
        if tok.item == Token::CloseBlock(BlockDelim::Parenthesis) {
            break;
        }

        let arg_name = expect_ident(tok)?;
        consume_token(Token::Colon, iter)?;
        func.add_argument(arg_name.map(Into::into), parse_type(iter)?.map(Some))?;

        let tok = iter.next_token();
        if tok.item == Token::CloseBlock(BlockDelim::Parenthesis) {
            break;
        } else if tok.item != Token::Comma {
            CompileError::expected(
                &[Token::Comma, Token::CloseBlock(BlockDelim::Parenthesis)],
                &tok,
            )?;
        }
    }

    if try_consume_token(Token::Arrow, iter) {
        func.set_return(parse_type(iter)?);
    }

    consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;

    func.set_body(parse_block(None, iter)?);

    Ok(func)
}

/// `use a::b::c`, `use` should already be consumed.
fn parse_import<'a>(
    at: &[&'a str],
    iter: &mut TokenIter<'a>,
) -> Result<Vec<Meta<'a, &'a str>>, CompileError> {
    let next = iter.next_token();
    let mut path = match next.item {
        Token::Ident(s) => {
            let mut path: Vec<_> = at.iter().copied().map(Meta::fake).collect();
            path.push(next.replace(s));
            path
        }
        _ => return CompileError::new(&next, "Invalid path segment"),
    };

    while try_consume_token(Token::DoubleColon, iter) {
        let next = iter.next_token();
        match next.item {
            Token::Ident(s) => path.push(next.replace(s)),
            _ => return CompileError::new(&next, "Invalid path segment"),
        }
    }

    consume_token(Token::SemiColon, iter)?;

    Ok(path)
}
