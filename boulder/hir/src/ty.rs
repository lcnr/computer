use super::*;

#[derive(Debug, Clone)]
pub struct Type {
    pub name: Box<str>,
    pub kind: Kind,
}

#[derive(Debug, Clone)]
pub enum Kind {
    Empty,
    U8,
    U16,
    U32,
}
