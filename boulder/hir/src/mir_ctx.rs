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
    if let Some(ref old) = *old {
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

fn check_bool_binop<'a>(
    def: &FunctionDefinition<'a, TypeId>,
    ty: TypeId,
    s: &str,
) -> Result<(), CompileError> {
    if def.args.len() == 2
        && def.args.iter().zip(&[ty, ty]).all(|(a, &b)| a.item == b)
        && def.ty.item == BOOL_TYPE_ID
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
    add32: Option<Meta<'a, FunctionId>>,
    add16: Option<Meta<'a, FunctionId>>,
    sub32: Option<Meta<'a, FunctionId>>,
    sub16: Option<Meta<'a, FunctionId>>,
    shl32: Option<Meta<'a, FunctionId>>,
    shl16: Option<Meta<'a, FunctionId>>,
    shr32: Option<Meta<'a, FunctionId>>,
    shr16: Option<Meta<'a, FunctionId>>,
    gt32: Option<Meta<'a, FunctionId>>,
    gt16: Option<Meta<'a, FunctionId>>,
    gte32: Option<Meta<'a, FunctionId>>,
    gte16: Option<Meta<'a, FunctionId>>,
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
                    FunctionAttribute::LangItem(LangItem::Add32) => {
                        check_binop(def, U32_TYPE_ID, "add32")?;
                        insert_lang_item(&mut builder.add32, attr.replace(id), "add32")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Add16) => {
                        check_binop(def, U16_TYPE_ID, "add16")?;
                        insert_lang_item(&mut builder.add16, attr.replace(id), "add16")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Sub32) => {
                        check_binop(def, U32_TYPE_ID, "sub32")?;
                        insert_lang_item(&mut builder.sub32, attr.replace(id), "sub32")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Sub16) => {
                        check_binop(def, U16_TYPE_ID, "sub16")?;
                        insert_lang_item(&mut builder.sub16, attr.replace(id), "sub16")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Shl32) => {
                        check_binop(def, U32_TYPE_ID, "shl32")?;
                        insert_lang_item(&mut builder.shl32, attr.replace(id), "shl32")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Shl16) => {
                        check_binop(def, U16_TYPE_ID, "shl16")?;
                        insert_lang_item(&mut builder.shl16, attr.replace(id), "shl16")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Shr32) => {
                        check_binop(def, U32_TYPE_ID, "shr32")?;
                        insert_lang_item(&mut builder.shr32, attr.replace(id), "shr32")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Shr16) => {
                        check_binop(def, U16_TYPE_ID, "shr16")?;
                        insert_lang_item(&mut builder.shr16, attr.replace(id), "shr16")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Gt32) => {
                        check_bool_binop(def, U32_TYPE_ID, "gt32")?;
                        insert_lang_item(&mut builder.gt32, attr.replace(id), "gt32")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Gt16) => {
                        check_bool_binop(def, U16_TYPE_ID, "gt16")?;
                        insert_lang_item(&mut builder.gt16, attr.replace(id), "gt16")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Gte32) => {
                        check_bool_binop(def, U32_TYPE_ID, "gte32")?;
                        insert_lang_item(&mut builder.gte32, attr.replace(id), "gte32")?;
                        func.attributes.remove(i);
                    }
                    FunctionAttribute::LangItem(LangItem::Gte16) => {
                        check_bool_binop(def, U16_TYPE_ID, "gte16")?;
                        insert_lang_item(&mut builder.gte16, attr.replace(id), "gte16")?;
                        func.attributes.remove(i);
                    }
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
        builder.into_ctx()
    }

    fn into_ctx(self) -> Result<Context, CompileError> {
        fn unwrap_item<'a, T>(item: Option<Meta<'a, T>>, s: &str) -> Result<T, CompileError> {
            if let Some(i) = item {
                Ok(i.item)
            } else {
                CompileError::without_location(format_args!("Missing `lang_item: `{}`", s))
            }
        }

        Ok(Context {
            add32: unwrap_item(self.add32, "add32")?,
            add16: unwrap_item(self.add16, "add16")?,
            sub32: unwrap_item(self.sub32, "sub32")?,
            sub16: unwrap_item(self.sub16, "sub16")?,
            shl32: unwrap_item(self.shl32, "shl32")?,
            shl16: unwrap_item(self.shl16, "shl16")?,
            shr32: unwrap_item(self.shr32, "shr32")?,
            shr16: unwrap_item(self.shr16, "shr16")?,
            gt32: unwrap_item(self.gt32, "gt32")?,
            gt16: unwrap_item(self.gt16, "gt16")?,
            gte32: unwrap_item(self.gte32, "gte32")?,
            gte16: unwrap_item(self.gte16, "gte16")?,
            div32: unwrap_item(self.div32, "div32")?,
            div16: unwrap_item(self.div16, "div16")?,
            div8: unwrap_item(self.div8, "div8")?,
            rem32: unwrap_item(self.rem32, "rem32")?,
            rem16: unwrap_item(self.rem16, "rem16")?,
            rem8: unwrap_item(self.rem8, "rem8")?,
            mul32: unwrap_item(self.mul32, "mul32")?,
            mul16: unwrap_item(self.mul16, "mul16")?,
            mul8: unwrap_item(self.mul8, "mul8")?,
            true_replacement: u8::max_value(),
            false_replacement: u8::max_value(),
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

                    if let Some(ref should_test) = builder.is_test {
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
                    if let Some(ref should_export) = builder.export {
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
                    if let Some(ref hidden) = builder.hidden {
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

        if !func.attributes.is_empty() {
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
            builder.into_ctx()
        }
    }

    fn into_ctx(self) -> Result<FunctionContext, CompileError> {
        Ok(FunctionContext {
            is_test: self.is_test.is_some(),
            export: self.export.is_some(),
            hidden: self.hidden.is_some(),
        })
    }
}
