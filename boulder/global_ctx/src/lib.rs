use typed_arena::Arena;

pub struct GlobalCtx {
    strings: Arena<Box<str>>,
}

impl GlobalCtx {
    pub fn new() -> Self {
        Self {
            strings: Arena::new(),
        }
    }

    pub fn insert_str<'a>(&'a self, s: Box<str>) -> &'a str {
        self.strings.alloc(s)
    }
}
