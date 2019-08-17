use hir::HIR;

pub mod tokenize;

#[derive(Debug)]
pub struct ParseError;

pub fn parse(src: &str) -> Result<HIR, ParseError> {
    for tok in tokenize::TokenIter::new(src) {
        println!("{:?}", tok);
    }
    unimplemented!();
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
