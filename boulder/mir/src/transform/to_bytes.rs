use std::iter;

use tindex::TVec;

use shared_id::{U16_TYPE_ID, U32_TYPE_ID, U8_TYPE_ID, BOOL_TYPE_ID};

use crate::{
    Action, Binop, BlockId, FieldId, Function, Mir, Object, Step, StepId, Type, UnaryOperation,
};

impl<'a> Mir<'a> {
    /// reduce all `u32` and `u16` to `u8`
    pub fn reduce_to_bytes(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::reduce_sum_types");
        self.types[U32_TYPE_ID] =
            Type::Struct(tvec![U8_TYPE_ID, U8_TYPE_ID, U8_TYPE_ID, U8_TYPE_ID]);
        self.types[U16_TYPE_ID] = Type::Struct(tvec![U8_TYPE_ID, U8_TYPE_ID]);
        for func in self.functions.iter_mut() {
            func.reduce_to_bytes();
        }
    }
}

fn i(a: StepId, b: StepId) -> impl Iterator<Item = StepId> {
    iter::once(a).chain(iter::once(b))
}

fn binop_byte(new_steps: &mut TVec<StepId, Step>, field: usize) -> (StepId, StepId) {
    let a = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(field)),
    ));
    let b = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(1), FieldId::from(field)),
    ));

    (a, b)
}

fn invert_u16() -> TVec<StepId, Step> {
    let mut new_steps = TVec::new();
    let high = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(0)),
    ));
    let high_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, high),
    ));
    let low = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(1)),
    ));
    let low_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, low),
    ));
    new_steps.push(Step::new(
        U16_TYPE_ID,
        Action::InitializeStruct(tvec![high_inv, low_inv]),
    ));

    new_steps
}

fn invert_u32() -> TVec<StepId, Step> {
    let mut new_steps = TVec::new();
    let high = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(0)),
    ));
    let high_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, high),
    ));
    let high_middle = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(1)),
    ));
    let high_middle_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, high_middle),
    ));
    let low_middle = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(2)),
    ));
    let low_middle_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, low_middle),
    ));
    let low = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::StructFieldAccess(StepId::replacement(0), FieldId::from(3)),
    ));
    let low_inv = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::UnaryOperation(UnaryOperation::Invert, low),
    ));
    new_steps.push(Step::new(
        U32_TYPE_ID,
        Action::InitializeStruct(tvec![high_inv, high_middle_inv, low_middle_inv, low_inv]),
    ));
    new_steps
}

fn eq_u16() -> TVec<StepId, Step> {
    let mut new_steps = TVec::new();
    let (high_a, high_b) = binop_byte(&mut new_steps, 0);
    let high_eq = new_steps.push(Step::new(
        BOOL_TYPE_ID,
        Action::Binop(Binop::Eq, high_a, high_b),
    ));

    let (low_a, low_b) = binop_byte(&mut new_steps, 1);
    let low_eq = new_steps.push(Step::new(
        BOOL_TYPE_ID,
        Action::Binop(Binop::Eq, low_a, low_b),
    ));
    new_steps.push(Step::new(
        BOOL_TYPE_ID,
        Action::Binop(Binop::BitAnd, high_eq, low_eq),
    ));

    new_steps
}

fn eq_u32() -> TVec<StepId, Step> {
    let mut new_steps = TVec::new();

    let (high_a, high_b) = binop_byte(&mut new_steps, 0);
    let high_eq = new_steps.push(Step::new(
        BOOL_TYPE_ID,
        Action::Binop(Binop::Eq, high_a, high_b),
    ));

    let (high_middle_a, high_middle_b) = binop_byte(&mut new_steps, 1);
    let high_middle_eq = new_steps.push(Step::new(
        BOOL_TYPE_ID,
        Action::Binop(Binop::Eq, high_middle_a, high_middle_b),
    ));

    let high_unified = new_steps.push(Step::new(
        BOOL_TYPE_ID,
        Action::Binop(Binop::BitAnd, high_eq, high_middle_eq),
    ));

    let (low_middle_a, low_middle_b) = binop_byte(&mut new_steps, 2);
    let low_middle_eq = new_steps.push(Step::new(
        BOOL_TYPE_ID,
        Action::Binop(Binop::Eq, low_middle_a, low_middle_b),
    ));

    let (low_a, low_b) = binop_byte(&mut new_steps, 3);
    let low_eq = new_steps.push(Step::new(
        BOOL_TYPE_ID,
        Action::Binop(Binop::Eq, low_a, low_b),
    ));

    let low_unified = new_steps.push(Step::new(
        BOOL_TYPE_ID,
        Action::Binop(Binop::BitAnd, low_eq, low_middle_eq),
    ));

    new_steps.push(Step::new(
        BOOL_TYPE_ID,
        Action::Binop(Binop::BitAnd, high_unified, low_unified),
    ));

    new_steps
}

fn bytewise_u16(op: Binop) -> TVec<StepId, Step> {
    let mut new_steps = TVec::new();
    let (high_a, high_b) = binop_byte(&mut new_steps, 0);
    let high_op = new_steps.push(Step::new(U8_TYPE_ID, Action::Binop(op, high_a, high_b)));

    let (low_a, low_b) = binop_byte(&mut new_steps, 1);
    let low_op = new_steps.push(Step::new(U8_TYPE_ID, Action::Binop(op, low_a, low_b)));
    new_steps.push(Step::new(
        U16_TYPE_ID,
        Action::InitializeStruct(tvec![high_op, low_op]),
    ));

    new_steps
}

fn bytewise_u32(op: Binop) -> TVec<StepId, Step> {
    let mut new_steps = TVec::new();

    let (high_a, high_b) = binop_byte(&mut new_steps, 0);
    let high_op = new_steps.push(Step::new(U8_TYPE_ID, Action::Binop(op, high_a, high_b)));

    let (high_middle_a, high_middle_b) = binop_byte(&mut new_steps, 1);
    let high_middle_op = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::Binop(op, high_middle_a, high_middle_b),
    ));

    let (low_middle_a, low_middle_b) = binop_byte(&mut new_steps, 2);
    let low_middle_op = new_steps.push(Step::new(
        U8_TYPE_ID,
        Action::Binop(op, low_middle_a, low_middle_b),
    ));

    let (low_a, low_b) = binop_byte(&mut new_steps, 3);
    let low_op = new_steps.push(Step::new(U8_TYPE_ID, Action::Binop(op, low_a, low_b)));

    new_steps.push(Step::new(
        U32_TYPE_ID,
        Action::InitializeStruct(tvec![high_op, high_middle_op, low_middle_op, low_op]),
    ));
    new_steps
}

impl<'a> Function<'a> {
    fn reduce_to_bytes(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Function::reduce_sum_types");
        let mut block_id = BlockId::from(0);
        while block_id.0 < self.blocks.len() {
            let block = &mut self.blocks[block_id];
            let mut s_id = StepId::from(0);
            while s_id.0 < block.steps.len() {
                let steps = &mut block.steps;
                match &mut steps[s_id].action {
                    &mut Action::LoadConstant(ref mut obj) => {
                        obj.reduce_to_bytes();
                    }
                    &mut Action::UnaryOperation(op, target_id) => {
                        let new_steps = match (steps[target_id].ty, op) {
                            (U16_TYPE_ID, UnaryOperation::Invert) => invert_u16(),
                            (U32_TYPE_ID, UnaryOperation::Invert) => invert_u32(),
                            (_, UnaryOperation::Invert) => {
                                s_id.0 += 1;
                                continue;
                            }
                        };
                        s_id = block.insert_steps(s_id..=s_id, new_steps, iter::once(target_id));
                    }
                    &mut Action::Binop(op, a, b) => match (steps[a].ty, steps[b].ty, op) {
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Add) => unimplemented!("u16 add"),
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Add) => unimplemented!("u32 add"),
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Sub) => unimplemented!("u16 sub"),
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Sub) => unimplemented!("u32 sub"),
                        (_, _, Binop::Mul) | (_, _, Binop::Div) | (_, _, Binop::Rem) => {
                            unreachable!("to_bytes called with extended binops: {:?}", op)
                        }
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Shl) => unimplemented!("u16 shl"),
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Shl) => unimplemented!("u32 shl"),
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Shr) => unimplemented!("u16 shr"),
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Shr) => unimplemented!("u32 shr"),
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Eq) => {
                            s_id = block.replace_step(s_id, eq_u16(), i(a, b))
                        }
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Eq) => {
                            s_id = block.replace_step(s_id, eq_u32(), i(a, b))
                        }
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Neq) => unimplemented!("u16 neq"),
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Neq) => unimplemented!("u32 neq"),
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Gt) => unimplemented!("u16 gt"),
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Gt) => unimplemented!("u32 gt"),
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::Gte) => unimplemented!("u16 gte"),
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::Gte) => unimplemented!("u32 gte"),
                        (U16_TYPE_ID, U16_TYPE_ID, Binop::BitOr)
                        | (U16_TYPE_ID, U16_TYPE_ID, Binop::BitAnd) => {
                            s_id = block.replace_step(s_id, bytewise_u16(op), i(a, b))
                        }
                        (U32_TYPE_ID, U32_TYPE_ID, Binop::BitOr)
                        | (U32_TYPE_ID, U32_TYPE_ID, Binop::BitAnd) => {
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
                        | (_, _, Binop::BitOr) => s_id.0 += 1,
                    },
                    _ => s_id.0 += 1,
                }
            }
            block_id.0 += 1;
        }
    }
}

impl Object {
    fn reduce_to_bytes(&mut self) {
        match self {
            &mut Object::Unit | &mut Object::U8(_) | &mut Object::Undefined => (),
            &mut Object::U16(val) => {
                *self = Object::Struct(val.to_be_bytes().iter().copied().map(Object::U8).collect())
            }
            &mut Object::U32(val) => {
                *self = Object::Struct(val.to_be_bytes().iter().copied().map(Object::U8).collect())
            }
            &mut Object::Struct(ref mut content) => {
                content.iter_mut().for_each(Self::reduce_to_bytes)
            }
            &mut Object::Variant(_, ref mut obj) => obj.reduce_to_bytes(),
            &mut Object::Field(_, ref mut obj) => obj.reduce_to_bytes(),
        }
    }
}
