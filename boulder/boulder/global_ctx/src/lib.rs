use std::cell::UnsafeCell;

pub struct GlobalCtx {
    strings: UnsafeCell<Vec<Pin<Box<str>>>>,

}