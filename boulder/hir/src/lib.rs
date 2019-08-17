pub struct Field {
    name: Box<str>,
}

pub struct Struct {
    name: Box<str>,
    fields: Vec<Field>,
}

pub struct Function {
    name: Box<str>,
    arguments: Vec<(Box<str>, Box<str>)>,
    ret: Option<Box<str>>,
}

impl Function {
    pub fn new(name: impl Into<Box<str>>) -> Self {
        Self {
            name: name.into(),
            arguments: Vec::new(),
            ret: None,
        }
    }

    pub fn add_argument(&mut self, name: Box<str>, ty: Box<str>) {
        self.arguments.push((name, ty));
    }

    pub fn add_ret(&mut self, ty: Box<str>) {
        self.ret = Some(ty);
    }
}

#[derive(Default)]
pub struct HIR {
    structs: Vec<Struct>,
    functions: Vec<Function>,
}

impl HIR {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_function(&mut self, func: Function) {
        self.functions.push(func);
    }
}
