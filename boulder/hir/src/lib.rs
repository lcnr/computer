pub struct Field<'a> {
    name: &'a str,
}

pub struct Struct<'a> {
    name: &'a str,
    fields: &'a str,
}

pub struct Function<'a> {
    name: &'a str,
}

pub struct HIR<'a> {
    structs: Vec<Struct<'a>>,
    functions: Vec<Function<'a>>,
}
