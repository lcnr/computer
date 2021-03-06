use std::collections::hash_map::{Entry, HashMap};

use diagnostics::{CompileError, Meta};

use shared_id::{FunctionId, TypeId};

#[derive(Debug, Clone)]
pub struct Module<'a> {
    pub meta: Meta<'a, ()>,
    pub functions: HashMap<&'a str, FunctionId>,
    pub types: HashMap<Box<str>, TypeId>,
    pub modules: HashMap<&'a str, Module<'a>>,
}

impl<'a> Module<'a> {
    pub fn new(meta: Meta<'a, ()>) -> Self {
        Self {
            meta,
            functions: HashMap::new(),
            types: HashMap::new(),
            modules: HashMap::new(),
        }
    }

    pub fn add_module(
        &mut self,
        at: &[&'a str],
        name: Meta<'a, &'a str>,
    ) -> Result<(), Meta<'a, ()>> {
        if let Some((first, rest)) = at.split_first() {
            let inner = self.modules.get_mut(first).unwrap();
            inner.add_module(rest, name)
        } else {
            let meta = name.simplify();
            match self.modules.entry(name.item) {
                Entry::Occupied(o) => Err(o.get().meta.clone()),
                Entry::Vacant(entry) => {
                    entry.insert(Module::new(meta));
                    Ok(())
                }
            }
        }
    }

    pub fn add_import(
        &mut self,
        at: &[&'a str],
        import: &[Meta<'a, &'a str>],
    ) -> Result<(), CompileError> {
        if let Some((last, rest)) = import.split_last() {
            let mut current = &*self;
            for step in rest.iter() {
                if let Some(inner) = current.modules.get(step.item) {
                    current = &inner;
                } else {
                    return CompileError::new(
                        &step,
                        format_args!("The sub module `{}` does not exist.", step.item),
                    );
                }
            }

            let f = current.functions.get(last.item).copied();
            let t = current.types.get(last.item).copied();
            if let Some(f) = f {
                self.add_function(at, last.item, f).or_else(|_| {
                    CompileError::new(
                        &last,
                        format_args!(
                            "`{}` is an already valid function name in the current module",
                            last.item
                        ),
                    )
                })?;
            }
            if let Some(t) = t {
                self.add_type(at, Box::from(last.item), t).or_else(|_| {
                    CompileError::new(
                        &last,
                        format_args!(
                            "`{}` is an already valid type name in the current module",
                            last.item
                        ),
                    )
                })?;
            }

            if f.is_none() && t.is_none() {
                CompileError::new(
                    &last,
                    format_args!("Invalid import, the item `{}` does not exist", last.item),
                )
            } else {
                Ok(())
            }
        } else {
            unreachable!("empty import");
        }
    }

    pub fn add_type(&mut self, at: &[&'a str], name: Box<str>, id: TypeId) -> Result<(), TypeId> {
        if let Some((first, rest)) = at.split_first() {
            let inner = self.modules.get_mut(first).unwrap();
            inner.add_type(rest, name, id)
        } else {
            match self.types.entry(name) {
                Entry::Occupied(o) => Err(*o.get()),
                Entry::Vacant(entry) => {
                    entry.insert(id);
                    Ok(())
                }
            }
        }
    }

    pub fn get_type(&self, at: &[&'a str], name: &str) -> Option<TypeId> {
        if let Some((first, rest)) = at.split_first() {
            let inner = self.modules.get(first).unwrap();
            if let Some(id) = inner.get_type(rest, name) {
                return Some(id);
            }
        }

        self.types.get(name).copied()
    }

    pub fn add_function(
        &mut self,
        at: &[&'a str],
        name: &'a str,
        id: FunctionId,
    ) -> Result<(), FunctionId> {
        if let Some((first, rest)) = at.split_first() {
            let inner = self.modules.get_mut(first).unwrap();
            inner.add_function(rest, name, id)
        } else {
            match self.functions.entry(name) {
                Entry::Occupied(o) => Err(*o.get()),
                Entry::Vacant(entry) => {
                    entry.insert(id);
                    Ok(())
                }
            }
        }
    }

    pub fn get_function(&self, at: &[&'a str], name: &str) -> Option<FunctionId> {
        if let Some((first, rest)) = at.split_first() {
            let inner = self.modules.get(first).unwrap();
            if let Some(id) = inner.get_function(rest, name) {
                return Some(id);
            }
        }

        self.functions.get(name).copied()
    }
}
