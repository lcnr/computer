use std::collections::hash_map::{Entry, HashMap};

use shared_id::{FunctionId, TypeId};

#[derive(Debug, Clone)]
pub struct Module {
    pub functions: HashMap<Box<str>, FunctionId>,
    pub types: HashMap<Box<str>, TypeId>,
    pub modules: HashMap<Box<str>, Module>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            types: HashMap::new(),
            modules: HashMap::new(),
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
