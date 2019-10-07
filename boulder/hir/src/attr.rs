use diagnostics::{CompileError, Meta};

#[derive(Debug, Clone)]
pub enum TypeAttribute<'a> {
    Str(&'a str),
}

impl<'a> TypeAttribute<'a> {
    pub fn new(
        _name: Meta<'a, &'a str>,
        _args: Vec<Meta<'a, &'a str>>,
    ) -> Result<Meta<'a, Self>, CompileError> {
        unimplemented!()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LangItem {
    Div32,
    Div16,
    Div8,
    Rem32,
    Rem16,
    Rem8,
    Mul32,
    Mul16,
    Mul8,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FunctionAttribute<'a> {
    LangItem(LangItem),
    TestFn,
    Str(&'a str),
}

impl<'a> FunctionAttribute<'a> {
    pub fn new(
        name: Meta<'a, &'a str>,
        args: Vec<Meta<'a, &'a str>>,
    ) -> Result<Meta<'a, Self>, CompileError> {
        match name.item {
            "lang_item" => {
                if args.len() == 1 {
                    match args[0].item {
                        "div32" => Ok(args[0].replace(FunctionAttribute::LangItem(LangItem::Div32))),
                        "div16" => Ok(args[0].replace(FunctionAttribute::LangItem(LangItem::Div16))),
                        "div8" => Ok(args[0].replace(FunctionAttribute::LangItem(LangItem::Div8))),
                        "rem32" => Ok(args[0].replace(FunctionAttribute::LangItem(LangItem::Rem32))),
                        "rem16" => Ok(args[0].replace(FunctionAttribute::LangItem(LangItem::Rem16))),
                        "rem8" => Ok(args[0].replace(FunctionAttribute::LangItem(LangItem::Rem8))),
                        "mul32" => Ok(args[0].replace(FunctionAttribute::LangItem(LangItem::Mul32))),
                        "mul16" => Ok(args[0].replace(FunctionAttribute::LangItem(LangItem::Mul16))),
                        "mul8" => Ok(args[0].replace(FunctionAttribute::LangItem(LangItem::Mul8))),
                        _ => CompileError::new(
                            &args[0],
                            format_args!("Unknown `lang_item`: `{}`", args[0].item),
                        ),
                    }
                } else {
                    CompileError::new(
                        &name,
                        format_args!(
                            "Invalid `lang_item`. Expected 1 argument, found {}",
                            args.len()
                        ),
                    )
                }
            }
            "test" => Ok(name.replace(FunctionAttribute::TestFn)),
            _ => CompileError::new(&name, format_args!("Unknown attribute `{}`", name.item)),
        }
    }
}
