// A wrapper storing metadata
pub struct Meta<'a, T> {
    pub data: T,
    pub origin: &'a str,
    pub file: &'a str,
    pub line: u32,
}
