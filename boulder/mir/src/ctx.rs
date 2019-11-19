use shared_id::FunctionId;

#[derive(Debug, Clone)]
pub struct Context {
    pub u16b0: FunctionId,
    pub u16b1: FunctionId,
    pub u32b0: FunctionId,
    pub u32b1: FunctionId,
    pub u32b2: FunctionId,
    pub u32b3: FunctionId,
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
