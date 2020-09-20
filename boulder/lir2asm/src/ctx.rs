use shared_id::{BlockId, LocationId, TagId};

use tindex::TVec;

#[derive(Debug)]
pub struct Context {
    pub stack: TagId,
    pub scratch_space: TagId,
    pub tm: TagManager,
}

#[derive(Debug)]
pub struct FunctionData {
    pub blocks: TVec<BlockId, TagId>,
    pub storage: TVec<LocationId, TagId>,
}

impl Context {
    pub fn new() -> Self {
        let mut tm = TagManager::new();
        let stack = tm.next();
        let scratch_space = tm.next();
        Context {
            stack,
            scratch_space,
            tm,
        }
    }
}

#[derive(Debug)]
pub struct TagManager {
    tag: TagId,
}

impl TagManager {
    pub fn new() -> Self {
        TagManager { tag: TagId(0) }
    }

    pub fn next(&mut self) -> TagId {
        let tag = self.tag;
        self.tag = tag + 1;
        tag
    }
}
