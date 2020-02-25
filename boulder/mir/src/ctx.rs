use shared_id::FunctionId;

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
    pub true_replacement: u8,
    pub false_replacement: u8,
}

#[derive(Debug, Clone)]
pub struct FunctionContext {
    pub export: bool,
    pub is_test: bool,
    pub hidden: bool,
}
