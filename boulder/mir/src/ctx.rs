use shared_id::FunctionId;

#[derive(Debug, Clone)]
pub struct Context {
    pub div32: FunctionId,
    pub div16: FunctionId,
    pub div8: FunctionId,
    pub rem32: FunctionId,
    pub rem16: FunctionId,
    pub rem8: FunctionId,
    pub mul32: FunctionId,
    pub mul16: FunctionId,
    pub mul8: FunctionId,
}

#[derive(Debug, Clone)]
pub struct FunctionContext {
    pub export: bool,
    pub is_test: bool,
}
