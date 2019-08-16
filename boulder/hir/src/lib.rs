pub struct Field<'a> {
    name: &'a str,
}

pub struct Struct<'a> {
    fields: &'a str,
    
}

pub struct Function {

}

pub struct HIR {
    structs: Vec<Struct>,
    functions: Vec<Function>,
}
