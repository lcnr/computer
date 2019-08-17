use hir::HIR;

use boulder_core::Meta;

mod error;
mod tokenize;

use error::ParseError;

use tokenize::{BlockDelim, Keyword, Operator, Token, TokenIter};

type ParseResult<T> = Result<T, ParseError>;

pub fn parse(src: &str) -> Result<HIR, ParseError> {
    let iter = &mut TokenIter::new(src);
    let mut hir = HIR::new();
    while let Some(token) = iter.next() {
        match token.item {
            Token::Keyword(Keyword::Function) => {
                hir.add_function(parse_function(iter)?);
            }
            _ => unimplemented!(),
        }
        println!("{:?}", token);
    }

    unimplemented!()
}

fn consume_token(expected: Token, iter: &mut TokenIter) -> ParseResult<()> {
    let tok = iter.next().unwrap();
    if expected == tok.item {
        Ok(())
    } else {
        ParseError::expected(&expected, &tok)
    }
}

fn expect_ident(tok: Meta<'_, Token>) -> ParseResult<Box<str>> {
    if let Token::Ident(value) = tok.item {
        Ok(value)
    } else {
        ParseError::expected(&"Ident", &tok)
    }
}

// parse a function, `fn` should already be consumed
fn parse_function(iter: &mut TokenIter) -> Result<hir::Function, ParseError> {
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
            ParseError::expected(&[Token::Comma, Token::CloseBlock(BlockDelim::Parenthesis)], &tok)?;
        }
    }

    let tok = iter.next().unwrap();
    if tok.item == Token::Arrow {
        func.add_ret(expect_ident(iter.next().unwrap())?);
        consume_token(Token::OpenBlock(BlockDelim::Brace), iter)?;
    } else if tok.item != Token::OpenBlock(BlockDelim::Brace) {
        ParseError::expected(&Token::OpenBlock(BlockDelim::Brace), &tok)?;
    }

    


    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function() {
        parse(
            "fn added(a: u32, b: u32) -> u32 {
                a + b
            }",
        )
        .unwrap();
    }
}
