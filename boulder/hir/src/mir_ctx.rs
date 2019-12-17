use tindex::TSlice;

use diagnostics::{CompileError, Meta};

use shared_id::{FunctionId, BOOL_TYPE_ID, U16_TYPE_ID, U32_TYPE_ID, U8_TYPE_ID};

use mir::ctx::{Context, FunctionContext};

use crate::{
    attr::{FunctionAttribute, LangItem},
    func::{Function, FunctionDefinition},
    traits::{ResolvedIdentifiers, ResolvedTypes},
    Hir, TypeId,
};

fn insert_lang_item<'a, T>(
    old: &mut Option<Meta<'a, T>>,
    new: Meta<'a, T>,
    s: &str,
) -> Result<(), CompileError> {
    if let &mut Some(ref old) = old {
        CompileError::build(
            old,
            format_args!("`lang_item({})` defined more than once", s),
        )
        .with_location(&new)
        .build()
    } else {
        *old = Some(new);
        Ok(())
    }
}

fn check_binop<'a>(
    def: &FunctionDefinition<'a, TypeId>,
    ty: TypeId,
    s: &str,
) -> Result<(), CompileError> {
    if def.args.len() == 2
        && def.args.iter().zip(&[ty, ty]).all(|(a, &b)| a.item == b)
        && def.ty.item == ty
    {
        Ok(())
    } else {
        CompileError::new(
            &def.name,
            format_args!("Invalid function type for `lang_item({})`", s),
        )
    }
}

#[derive(Default)]
pub struct ContextBuilder<'a> {
    div32: Option<Meta<'a, FunctionId>>,
    div16: Option<Meta<'a, FunctionId>>,
    div8: Option<Meta<'a, FunctionId>>,
    rem32: Option<Meta<'a, FunctionId>>,
    rem16: Option<Meta<'a, FunctionId>>,
    rem8: Option<Meta<'a, FunctionId>>,
    mul32: Option<Meta<'a, FunctionId>>,
    mul16: Option<Meta<'a, FunctionId>>,
    mul8: Option<Meta<'a, FunctionId>>,
}

impl<'a> ContextBuilder<'a> {
    pub fn build(
        hir: &mut Hir<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>, TypeId, TypeId>,
        function_definitions: &TSlice<FunctionId, FunctionDefinition<'a, TypeId>>,
    ) -> Result<Context, CompileError> {
        #[cfg(feature = "profiler")]
        profile_scope!("ContextBuilder::build");
        let mut builder = ContextBuilder::default();

        for (id, func) in hir.functions.iter_mut().enumerate() {
            let id = FunctionId::from(id);
            let def = &function_definitions[id];

            for i in (0..func.attributes.len()).rev() {
                let attr = &func.attributes[i];
                match attr.item {
                    FunctionAttribute::LangItem(LangItem::Div32) => {
                        check_binop(def, U32_TYPE_ID, "div32")?;
                        insert_lang_item(&mut builder.div32, attr.replace(id), "div32")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Div16) => {
                        check_binop(def, U16_TYPE_ID, "div16")?;
                        insert_lang_item(&mut builder.div16, attr.replace(id), "div16")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Div8) => {
                        check_binop(def, U8_TYPE_ID, "div8")?;
                        insert_lang_item(&mut builder.div8, attr.replace(id), "div8")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Rem32) => {
                        check_binop(def, U32_TYPE_ID, "rem32")?;
                        insert_lang_item(&mut builder.rem32, attr.replace(id), "rem32")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Rem16) => {
                        check_binop(def, U16_TYPE_ID, "rem16")?;
                        insert_lang_item(&mut builder.rem16, attr.replace(id), "rem16")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Rem8) => {
                        check_binop(def, U8_TYPE_ID, "rem8")?;
                        insert_lang_item(&mut builder.rem8, attr.replace(id), "rem8")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Mul32) => {
                        check_binop(def, U32_TYPE_ID, "mul32")?;
                        insert_lang_item(&mut builder.mul32, attr.replace(id), "mul32")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Mul16) => {
                        check_binop(def, U16_TYPE_ID, "mul16")?;
                        insert_lang_item(&mut builder.mul16, attr.replace(id), "mul16")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Mul8) => {
                        check_binop(def, U8_TYPE_ID, "mul8")?;
                        insert_lang_item(&mut builder.mul8, attr.replace(id), "mul8")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::Hidden
                    | FunctionAttribute::Test
                    | FunctionAttribute::Export => (),
                    ref attr @ FunctionAttribute::Str(_) => unreachable!("{:?}", attr),
                }
            }
        }
        builder.to_ctx()
    }

    fn to_ctx(self) -> Result<Context, CompileError> {
        fn unwrap_item<'a, T>(item: Option<Meta<'a, T>>, s: &str) -> Result<T, CompileError> {
            if let Some(i) = item {
                Ok(i.item)
            } else {
                CompileError::without_location(format_args!("Missing `lang_item: `{}`", s))
            }
        }

        Ok(Context {
            div32: unwrap_item(self.div32, "div32")?,
            div16: unwrap_item(self.div16, "div16")?,
            div8: unwrap_item(self.div8, "div8")?,
            rem32: unwrap_item(self.rem32, "rem32")?,
            rem16: unwrap_item(self.rem16, "rem16")?,
            rem8: unwrap_item(self.rem8, "rem8")?,
            mul32: unwrap_item(self.mul32, "mul32")?,
            mul16: unwrap_item(self.mul16, "mul16")?,
            mul8: unwrap_item(self.mul8, "mul8")?,
        })
    }
}

#[derive(Debug, Default)]
pub struct FunctionContextBuilder<'a> {
    is_test: Option<Meta<'a, ()>>,
    export: Option<Meta<'a, ()>>,
    hidden: Option<Meta<'a, ()>>,
}

impl<'a> FunctionContextBuilder<'a> {
    pub fn build(
        func: &mut Function<'a, ResolvedIdentifiers<'a>, ResolvedTypes<'a>, TypeId>,
    ) -> Result<FunctionContext, CompileError> {
        #[cfg(feature = "profiler")]
        profile_scope!("FunctionContextBuilder::build");
        let mut builder = FunctionContextBuilder::default();

        for i in (0..func.attributes.len()).rev() {
            let attr = &func.attributes[i];
            match attr.item {
                FunctionAttribute::Test => {
                    if !func.arguments.is_empty() || func.ret.item != BOOL_TYPE_ID {
                        return CompileError::new(
                            &func.name,
                            format_args!("Invalid `test` function, expected `fn() -> Bool`"),
                        );
                    }

                    if let &Some(ref should_test) = &builder.is_test {
                        CompileError::build(
                            &attr,
                            "Attribute `test` used more than once on function",
                        )
                        .with_location(&should_test)
                        .build()?;
                    } else {
                        builder.is_test = Some(attr.simplify());
                        func.attributes.remove(i);
                    }
                }
                FunctionAttribute::Export => {
                    if let &Some(ref should_export) = &builder.export {
                        CompileError::build(
                            &attr,
                            "Attribute `export` used more than once on function",
                        )
                        .with_location(&should_export)
                        .build()?;
                    } else {
                        builder.export = Some(attr.simplify());
                        func.attributes.remove(i);
                    }
                }
                FunctionAttribute::Hidden => {
                    if let &Some(ref hidden) = &builder.hidden {
                        CompileError::build(
                            &attr,
                            "Attribute `hidden` used more than once on function",
                        )
                        .with_location(&hidden)
                        .build()?;
                    } else {
                        builder.hidden = Some(attr.simplify());
                        func.attributes.remove(i);
                    }
                }
                ref attr @ FunctionAttribute::Str(_)
                | ref attr @ FunctionAttribute::LangItem(_) => unreachable!("{:?}", attr),
            }
        }

        if func.attributes.len() != 0 {
            let mut builder = CompileError::build(
                &func.name,
                format_args!("Function with unknown attributes: `{}`", func.name.item),
            );

            for attr in func.attributes.iter() {
                builder = builder
                    .with_help(format_args!("Attribute `{}` is not known", attr.span_str()))
                    .with_location(attr);
            }
            builder.build()
        } else {
            builder.to_ctx()
        }
    }

    fn to_ctx(self) -> Result<FunctionContext, CompileError> {
        Ok(FunctionContext {
            is_test: self.is_test.is_some(),
            export: self.export.is_some(),
            hidden: self.hidden.is_some(),
        })
    }
}
