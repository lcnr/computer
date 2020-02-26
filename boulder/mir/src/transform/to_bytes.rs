use std::iter;

use tindex::TVec;

use shared_id::{
    BlockId, StepId, BOOL_TYPE_ID, U16_BYTES_TYPE_ID, U16_TYPE_ID, U32_BYTES_TYPE_ID, U32_TYPE_ID,
    U8_TYPE_ID,
};

use crate::{Action, Binop, Context, FieldId, Function, Mir, Object, Step, Type, UnaryOperation};

impl<'a> Mir<'a> {
    /// reduce all `u32` and `u16` to `u8`
    pub fn reduce_to_bytes(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::reduce_sum_types");
        self.types[U32_TYPE_ID] =
            Type::Struct(tvec![U8_TYPE_ID, U8_TYPE_ID, U8_TYPE_ID, U8_TYPE_ID]);
        self.types[U16_TYPE_ID] = Type::Struct(tvec![U8_TYPE_ID, U8_TYPE_ID]);

        for func in self.functions.iter_mut() {
            func.reduce_to_bytes(&self.ctx);
        }
    }
}

fn i(a: StepId, b: StepId) -> impl Iterator<Item = StepId> {
    iter::once(a).chain(iter::once(b))
}

fn byte(steps: &mut TVec<StepId, Step>, field: usize) -> StepId {
    steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(field)),
    ))
}

fn binop_byte(steps: &mut TVec<StepId, Step>, field: usize) -> (StepId, StepId) {
    let a = byte(steps, field);
    let b = steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(1), FieldId::from(field)),
    ));

    (a, b)
}

fn invert_u16() -> TVec<StepId, Step> {
    let mut steps = TVec::new();
    let low = byte(&mut steps, 0);
    let low_inv = steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, low),
    ));

    let high = byte(&mut steps, 1);
    let high_inv = steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, high),
    ));

    steps.push(Step::new(
        U16_TYPE_ID,
        Action::InitializeStruct(tvec![low_inv, high_inv]),
    ));

    steps
}

fn invert_u32() -> TVec<StepId, Step> {
    let mut steps = TVec::new();
    let a = byte(&mut steps, 0);
    let a_inv = steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, a),
    ));

    let b = byte(&mut steps, 1);
    let b_inv = steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, b),
    ));
    let c = byte(&mut steps, 2);
    let c_inv = steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, c),
    ));
    let d = byte(&mut steps, 3);
    let d_inv = steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, d),
    ));
    steps.push(Step::new(
        U32_TYPE_ID,
        Action::InitializeStruct(tvec![a_inv, b_inv, c_inv, d_inv]),
    ));
    steps
}

fn to_bytes_u16() -> TVec<StepId, Step> {
    let mut steps = TVec::new();
    let low = byte(&mut steps, 0);
    let high = byte(&mut steps, 1);

    steps.push(Step::new(
        U16_BYTES_TYPE_ID,
        Action::InitializeStruct(tvec![low, high]),
    ));

    steps
}

fn to_bytes_u32() -> TVec<StepId, Step> {
    let mut steps = TVec::new();

    let a = byte(&mut steps, 0);
    let b = byte(&mut steps, 1);
    let c = byte(&mut steps, 2);
    let d = byte(&mut steps, 3);

    steps.push(Step::new(
        U32_BYTES_TYPE_ID,
        Action::InitializeStruct(tvec![a, b, c, d]),
    ));

    steps
}

fn from_bytes_u16() -> TVec<StepId, Step> {
    let mut steps = TVec::new();
    let low = byte(&mut steps, 0);
    let high = byte(&mut steps, 1);

    steps.push(Step::new(
        U16_TYPE_ID,
        Action::InitializeStruct(tvec![low, high]),
    ));

    steps
}

fn from_bytes_u32() -> TVec<StepId, Step> {
    let mut steps = TVec::new();

    let a = byte(&mut steps, 0);
    let b = byte(&mut steps, 1);
    let c = byte(&mut steps, 2);
    let d = byte(&mut steps, 3);

    steps.push(Step::new(
        U32_TYPE_ID,
        Action::InitializeStruct(tvec![a, b, c, d]),
    ));

    steps
}

fn eq_u16() -> TVec<StepId, Step> {
    let mut steps = TVec::new();
    let (a_l, a_r) = binop_byte(&mut steps, 0);
    let a = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Eq, a_l, a_r)));

    let (b_l, b_r) = binop_byte(&mut steps, 1);
    let b = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Eq, b_l, b_r)));
    steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::BitAnd, a, b)));

    steps
}

fn eq_u32() -> TVec<StepId, Step> {
    let mut steps = TVec::new();

    let (a_l, a_r) = binop_byte(&mut steps, 0);
    let a = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Eq, a_l, a_r)));

    let (b_l, b_r) = binop_byte(&mut steps, 1);
    let b = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Eq, b_l, b_r)));

    let ab = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::BitAnd, a, b)));

    let (c_l, c_r) = binop_byte(&mut steps, 2);
    let c = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Eq, c_l, c_r)));

    let (d_l, d_r) = binop_byte(&mut steps, 3);
    let d = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Eq, d_l, d_r)));

    let cd = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::BitAnd, d, c)));

    steps.push(Step::new(
        BOOL_TYPE_ID,
        Action::Binop(Binop::BitAnd, ab, cd),
    ));

    steps
}

fn neq_u16() -> TVec<StepId, Step> {
    let mut steps = TVec::new();
    let (a_l, a_r) = binop_byte(&mut steps, 0);
    let a = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Neq, a_l, a_r)));

    let (b_l, b_r) = binop_byte(&mut steps, 1);
    let b = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Neq, b_l, b_r)));
    steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::BitOr, a, b)));

    steps
}

fn neq_u32() -> TVec<StepId, Step> {
    let mut steps = TVec::new();

    let (a_l, a_r) = binop_byte(&mut steps, 0);
    let a = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Neq, a_l, a_r)));

    let (b_l, b_r) = binop_byte(&mut steps, 1);
    let b = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Neq, b_l, b_r)));

    let ab = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::BitOr, a, b)));

    let (c_l, c_r) = binop_byte(&mut steps, 2);
    let c = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Neq, c_l, c_r)));

    let (d_l, d_r) = binop_byte(&mut steps, 3);
    let d = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::Neq, d_l, d_r)));

    let cd = steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::BitOr, d, c)));

    steps.push(Step::new(BOOL_TYPE_ID, Action::Binop(Binop::BitOr, ab, cd)));

    steps
}

fn bytewise_u16(op: Binop) -> TVec<StepId, Step> {
    let mut steps = TVec::new();
    let (a_l, a_r) = binop_byte(&mut steps, 0);
    let a = steps.push(Step::new(U8_TYPE_ID, Action::Binop(op, a_l, a_r)));

    let (b_l, b_r) = binop_byte(&mut steps, 1);
    let b = steps.push(Step::new(U8_TYPE_ID, Action::Binop(op, b_l, b_r)));
    steps.push(Step::new(
        U16_TYPE_ID,
        Action::InitializeStruct(tvec![a, b]),
    ));

    steps
}

fn bytewise_u32(op: Binop) -> TVec<StepId, Step> {
    let mut steps = TVec::new();

    let (a_l, a_r) = binop_byte(&mut steps, 0);
    let a = steps.push(Step::new(U8_TYPE_ID, Action::Binop(op, a_l, a_r)));

    let (b_l, b_r) = binop_byte(&mut steps, 1);
    let b = steps.push(Step::new(U8_TYPE_ID, Action::Binop(op, b_l, b_r)));

    let (c_l, c_r) = binop_byte(&mut steps, 2);
    let c = steps.push(Step::new(U8_TYPE_ID, Action::Binop(op, c_l, c_r)));

    let (d_l, d_r) = binop_byte(&mut steps, 3);
    let d = steps.push(Step::new(U8_TYPE_ID, Action::Binop(op, d_l, d_r)));

    steps.push(Step::new(
        U32_TYPE_ID,
        Action::InitializeStruct(tvec![a, b, c, d]),
    ));
    steps
}

impl<'a> Function<'a> {
    fn reduce_to_bytes(&mut self, ctx: &Context) {
        #[cfg(feature = "profiler")]
        profile_scope!("Function::reduce_sum_types");
        let mut block_id = BlockId::from(0);
        while block_id.0 < self.blocks.len() {
            let block = &mut self.blocks[block_id];
            let mut s_id = StepId(0);
            while s_id.0 < block.steps.len() {
                let steps = &mut block.steps;
                match steps[s_id].action {
                    Action::LoadConstant(ref mut obj) => {
                        obj.reduce_to_bytes();
                        s_id.0 += 1;
                    }
                    Action::UnaryOperation(op, target_id) => {
                        let new_steps = match (steps[target_id].ty, op) {
                            (U16_TYPE_ID, UnaryOperation::Invert) => invert_u16(),
                            (U32_TYPE_ID, UnaryOperation::Invert) => invert_u32(),
                            (_, UnaryOperation::Invert) | (_, UnaryOperation::Debug) => {
                                s_id.0 += 1;
                                continue;
                            }
                            (U16_TYPE_ID, UnaryOperation::ToBytes) => to_bytes_u16(),
                            (U32_TYPE_ID, UnaryOperation::ToBytes) => to_bytes_u32(),
                            (U16_BYTES_TYPE_ID, UnaryOperation::FromBytes) => from_bytes_u16(),
                            (U32_BYTES_TYPE_ID, UnaryOperation::FromBytes) => from_bytes_u32(),
                            (_, UnaryOperation::FromBytes) => unreachable!("invalid from_bytes"),
                            (_, UnaryOperation::ToBytes) => unreachable!("invalid to_bytes"),
                        };

                        s_id = block.insert_steps(s_id..=s_id, new_steps, iter::once(target_id))
                    }
                    Action::Binop(op, a, b) => match (steps[a].ty, steps[b].ty, op) {
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Add) => {
                            steps[s_id].action = Action::CallFunction(ctx.add16, vec![a, b])
                        }
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Add) => {
                            steps[s_id].action = Action::CallFunction(ctx.add32, vec![a, b])
                        }
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Sub) => {
                            steps[s_id].action = Action::CallFunction(ctx.sub16, vec![a, b])
                        }
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Sub) => {
                            steps[s_id].action = Action::CallFunction(ctx.sub32, vec![a, b])
                        }
                        (_, _, Binop::Mul) | (_, _, Binop::Div) | (_, _, Binop::Rem) => {
                            unreachable!("to_bytes called with extended binops: {:?}", op)
                        }
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Shl) => {
                            steps[s_id].action = Action::CallFunction(ctx.shl16, vec![a, b])
                        }
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Shl) => {
                            steps[s_id].action = Action::CallFunction(ctx.shl32, vec![a, b])
                        }
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Shr) => {
                            steps[s_id].action = Action::CallFunction(ctx.shr16, vec![a, b])
                        }
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Shr) => {
                            steps[s_id].action = Action::CallFunction(ctx.shr32, vec![a, b])
                        }
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Eq) => {
                            s_id = block.replace_step(s_id, eq_u16(), i(a, b))
                        }
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Eq) => {
                            s_id = block.replace_step(s_id, eq_u32(), i(a, b))
                        }
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Neq) => {
                            s_id = block.replace_step(s_id, neq_u16(), i(a, b))
                        }
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Neq) => {
                            s_id = block.replace_step(s_id, neq_u32(), i(a, b))
                        }
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Gt) => {
                            steps[s_id].action = Action::CallFunction(ctx.gt16, vec![a, b])
                        }
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Gt) => {
                            steps[s_id].action = Action::CallFunction(ctx.gt32, vec![a, b])
                        }
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Gte) => {
                            steps[s_id].action = Action::CallFunction(ctx.gte16, vec![a, b])
                        }
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Gte) => {
                            steps[s_id].action = Action::CallFunction(ctx.gte32, vec![a, b])
                        }
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::BitOr)
                        | (U16_TYPE_ID, U16_TYPE_ID, Binop::BitAnd)
                        | (U16_TYPE_ID, U16_TYPE_ID, Binop::BitXor) => {
                            s_id = block.replace_step(s_id, bytewise_u16(op), i(a, b))
                        }
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::BitOr)
                        | (U32_TYPE_ID, U32_TYPE_ID, Binop::BitAnd)
                        | (U32_TYPE_ID, U32_TYPE_ID, Binop::BitXor) => {
                            s_id = block.replace_step(s_id, bytewise_u32(op), i(a, b))
                        }
                        (_, _, Binop::BitAnd)
                        | (_, _, Binop::Sub)
                        | (_, _, Binop::Add)
                        | (_, _, Binop::Shl)
                        | (_, _, Binop::Shr)
                        | (_, _, Binop::Eq)
                        | (_, _, Binop::Neq)
                        | (_, _, Binop::Gt)
                        | (_, _, Binop::Gte)
                        | (_, _, Binop::BitOr)
                        | (_, _, Binop::BitXor) => s_id.0 += 1,
                    },
                    _ => s_id.0 += 1,
                }
            }
            block_id.0 += 1;
        }
    }
}

impl Object {
    pub fn reduce_to_bytes(&mut self) {
        match self {
            &mut Object::Unit | &mut Object::U8(_) | &mut Object::Undefined => (),
            &mut Object::U16(val) => {
                *self = Object::Struct(val.to_le_bytes().iter().copied().map(Object::U8).collect())
            }
            &mut Object::U32(val) => {
                *self = Object::Struct(val.to_le_bytes().iter().copied().map(Object::U8).collect())
            }
            &mut Object::Struct(ref mut content) => {
                content.iter_mut().for_each(Self::reduce_to_bytes)
            }
            &mut Object::Variant(_, ref mut obj) => obj.reduce_to_bytes(),
            &mut Object::Field(_, ref mut obj) => obj.reduce_to_bytes(),
        }
    }
}
