use shared_id::{FunctionId, FALSE_TYPE_ID, TRUE_TYPE_ID};

use crate::Object;

#[derive(Debug, Clone)]
pub struct Context {
    pub add32: FunctionId,
    pub add16: FunctionId,
    pub sub32: FunctionId,
    pub sub16: FunctionId,
    pub shl32: FunctionId,
    pub shl16: FunctionId,
    pub shr32: FunctionId,
    pub shr16: FunctionId,
    pub gt32: FunctionId,
    pub gt16: FunctionId,
    pub gte32: FunctionId,
    pub gte16: FunctionId,
    pub div32: FunctionId,
    pub div16: FunctionId,
    pub div8: FunctionId,
    pub rem32: FunctionId,
    pub rem16: FunctionId,
    pub rem8: FunctionId,
    pub mul32: FunctionId,
    pub mul16: FunctionId,
    pub mul8: FunctionId,
    /// have enums been converted to bytes
    pub e2b: bool,
    pub true_replacement: u8,
    pub false_replacement: u8,
}

impl Context {
    pub fn bool_to_object(&self, b: bool) -> Object {
        if self.e2b {
            if b {
                Object::U8(self.true_replacement)
            } else {
                Object::U8(self.false_replacement)
            }
        } else {
            Object::Variant(
                if b { TRUE_TYPE_ID } else { FALSE_TYPE_ID },
                Box::new(Object::Unit),
            )
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionContext {
    pub export: bool,
    pub is_test: bool,
    pub hidden: bool,
}
