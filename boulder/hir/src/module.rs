use std::collections::hash_map::{Entry, HashMap};

use diagnostics::Meta;

use shared_id::{FunctionId, TypeId};

#[derive(Debug, Clone)]
pub struct Module<'a> {
    pub meta: Meta<'a, ()>,
    pub functions: HashMap<Box<str>, FunctionId>,
    pub types: HashMap<Box<str>, TypeId>,
    pub modules: HashMap<Box<str>, Module<'a>>,
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
        at: &[Box<str>],
        name: Meta<'a, Box<str>>,
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

    pub fn add_type(&mut self, at: &[Box<str>], name: Box<str>, id: TypeId) -> Result<(), TypeId> {
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

    pub fn get_type(&self, at: &[Box<str>], name: &Box<str>) -> Option<TypeId> {
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
        at: &[Box<str>],
        name: Box<str>,
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

    pub fn get_function(&self, at: &[Box<str>], name: &Box<str>) -> Option<FunctionId> {
        if let Some((first, rest)) = at.split_first() {
            let inner = self.modules.get(first).unwrap();
            if let Some(id) = inner.get_function(rest, name) {
                return Some(id);
            }
        }

        self.functions.get(name).copied()
    }
}
