use std::iter;

use shared_id::{StepId, TypeId, FALSE_TYPE_ID, TRUE_TYPE_ID};
use tindex::TVec;

use crate::{binop::Binop, Action, Context, Mir, Object, Step, Type, UnaryOperation};

impl<'a> Mir<'a> {
    pub fn const_propagate(&mut self) {
        let ctx = &self.ctx;
        for func in self.functions.iter_mut() {
            for block in func.blocks.iter_mut() {
                let mut s = StepId(0);
                while s < block.steps.range_end() {
                    let step = &block.steps[s];
                    match step.action {
                        // TODO: consider computing functions in const propagate
                        Action::LoadInput(_)
                        | Action::LoadConstant(_)
                        | Action::CallFunction(_, _) => {}
                        Action::Extend(id) => {
                            if let Some(obj) = block.try_const(id) {
                                if matches!(obj, Object::Variant(_, _)) {
                                    block.steps[s].action = Action::LoadConstant(obj);
                                } else {
                                    block.steps[s].action = Action::LoadConstant(Object::Variant(
                                        block.steps[id].ty,
                                        Box::new(obj),
                                    ));
                                }
                            }
                        }
                        Action::Reduce(id) => {
                            if let Some(Object::Variant(ty, content)) = block.try_const(id) {
                                if let Type::Sum(cases) = &self.types[step.ty] {
                                    assert!(cases.get(ty), "undefined behavior");
                                    block.steps[s].action =
                                        Action::LoadConstant(Object::Variant(ty, content));
                                } else {
                                    assert_eq!(step.ty, ty, "undefined behavior");
                                    block.steps[s].action = Action::LoadConstant(*content);
                                }
                            }
                        }
                        Action::InitializeStruct(ref fields) => {
                            assert!(matches!(self.types[step.ty], Type::Struct(_)));
                            if let Some(fields) = fields
                                .iter()
                                .map(|&f| block.try_const(f))
                                .collect::<Option<_>>()
                            {
                                block.steps[s].action =
                                    Action::LoadConstant(Object::Struct(fields));
                            }
                        }
                        Action::InitializeUnion(id) => {
                            if let Some(obj) = block.try_const(id) {
                                block.steps[s].action = Action::LoadConstant(Object::Field(
                                    block.steps[id].ty,
                                    Box::new(obj),
                                ));
                            }
                        }
                        Action::StructFieldAccess(id, field) => {
                            if let Some(Object::Struct(mut fields)) = block.try_const(id) {
                                block.steps[s].action = Action::LoadConstant(fields.remove(field));
                            }
                        }
                        Action::UnionFieldAccess(id) => {
                            if let Some(Object::Field(_, obj)) = block.try_const(id) {
                                block.steps[s].action = Action::LoadConstant(*obj);
                            }
                        }
                        Action::UnaryOperation(op, id) => {
                            if let Some(obj) = block.try_const(id) {
                                match (op, obj) {
                                    (UnaryOperation::Debug, _) | (UnaryOperation::BlackBox, _) => {}
                                    (UnaryOperation::FromBytes, Object::Struct(fields)) => {
                                        block.steps[s].action = Action::LoadConstant(
                                            match fields.to_slice() {
                                                &[Object::U8(a), Object::U8(b)] => {
                                                    Object::U16(u16::from_le_bytes([a, b]))
                                                }
                                                &[Object::U8(a), Object::U8(b), Object::U8(c), Object::U8(d)] => {
                                                    Object::U32(u32::from_le_bytes([a, b, c, d]))
                                                }
                                                fields => unreachable!(
                                                    "invalid to_bytes argument: {:?}",
                                                    fields
                                                ),
                                            },
                                        )
                                    }
                                    (UnaryOperation::ToBytes, Object::U16(v)) => {
                                        block.steps[s].action =
                                            Action::LoadConstant(Object::Struct(
                                                v.to_le_bytes()
                                                    .iter()
                                                    .copied()
                                                    .map(Object::U8)
                                                    .collect(),
                                            ))
                                    }
                                    (UnaryOperation::ToBytes, Object::U32(v)) => {
                                        block.steps[s].action =
                                            Action::LoadConstant(Object::Struct(
                                                v.to_le_bytes()
                                                    .iter()
                                                    .copied()
                                                    .map(Object::U8)
                                                    .collect(),
                                            ))
                                    }
                                    (UnaryOperation::Invert, obj) => {
                                        block.steps[s].action = Action::LoadConstant(match obj {
                                            Object::U8(v) => Object::U8(!v),
                                            Object::U16(v) => Object::U16(!v),
                                            Object::U32(v) => Object::U32(!v),
                                            Object::Variant(TRUE_TYPE_ID, obj) => {
                                                Object::Variant(FALSE_TYPE_ID, obj)
                                            }
                                            Object::Variant(FALSE_TYPE_ID, obj) => {
                                                Object::Variant(TRUE_TYPE_ID, obj)
                                            }
                                            obj => unreachable!("invalid invert: {:?}", obj),
                                        })
                                    }
                                    _ => unreachable!("invalid unary operation"),
                                }
                            }
                        }
                        Action::Binop(op, a, b) => {
                            match (op, block.try_const(a), block.try_const(b)) {
                                (op, Some(a), Some(b)) => {
                                    block.steps[s].action = Action::LoadConstant(
                                        op.execute(ctx, a, b).expect("undefined"),
                                    )
                                }
                                (Binop::BitAnd, Some(obj), _) | (Binop::BitAnd, _, Some(obj))
                                    if obj.try_eval_to_int() == Some(0) =>
                                {
                                    block.steps[s].action = Action::LoadConstant(obj);
                                }
                                (Binop::BitOr, _, Some(Object::Variant(TRUE_TYPE_ID, _)))
                                | (Binop::BitOr, Some(Object::Variant(TRUE_TYPE_ID, _)), _) => {
                                    block.steps[s].action =
                                        Action::LoadConstant(ctx.bool_to_object(true))
                                }
                                (Binop::Gte, None, Some(obj)) => {
                                    if let Some(steps) = obj.try_eval_to_int().and_then(|value| {
                                        lte_const(ctx, block.steps[b].ty, value, obj.from_int_fn())
                                    }) {
                                        block.replace_step(s, steps, iter::once(a));
                                    }
                                }
                                (Binop::Gt, Some(obj), None) => {
                                    if let Some(steps) = obj.try_eval_to_int().and_then(|value| {
                                        gt_const(ctx, block.steps[a].ty, value, obj.from_int_fn())
                                    }) {
                                        block.replace_step(s, steps, iter::once(b));
                                    }
                                }
                                (Binop::Gt, None, Some(obj)) => {
                                    if let Some(steps) = obj.try_eval_to_int().and_then(|value| {
                                        lt_const(ctx, block.steps[b].ty, value, obj.from_int_fn())
                                    }) {
                                        block.replace_step(s, steps, iter::once(a));
                                    }
                                }
                                (Binop::Add, Some(obj), None)
                                    if obj.try_eval_to_int() == Some(0) =>
                                {
                                    block.replace_step_with_existing(s, b);
                                }
                                (Binop::Add, None, Some(obj))
                                | (Binop::Shl, None, Some(obj))
                                | (Binop::Shr, None, Some(obj))
                                    if obj.try_eval_to_int() == Some(0) =>
                                {
                                    block.replace_step_with_existing(s, a);
                                }
                                (Binop::Mul, Some(obj), None) => {
                                    if let Some(steps) = obj.try_eval_to_int().and_then(|value| {
                                        multiply_const(block.steps[s].ty, value, obj.from_int_fn())
                                    }) {
                                        block.replace_step(s, steps, iter::once(b));
                                    }
                                }
                                (Binop::Mul, None, Some(obj)) => {
                                    if let Some(steps) = obj.try_eval_to_int().and_then(|value| {
                                        multiply_const(block.steps[s].ty, value, obj.from_int_fn())
                                    }) {
                                        block.replace_step(s, steps, iter::once(a));
                                    }
                                }
                                (Binop::Div, Some(obj), None)
                                    if obj.try_eval_to_int() == Some(0) =>
                                {
                                    block.steps[s].action = Action::LoadConstant(obj);
                                }
                                (Binop::Div, None, Some(obj)) => {
                                    if let Some(steps) = obj.try_eval_to_int().and_then(|value| {
                                        div_const(block.steps[s].ty, value, obj.from_int_fn())
                                    }) {
                                        block.replace_step(s, steps, iter::once(a));
                                    }
                                }
                                _ => (),
                            }
                        }
                    }
                    s = s + 1;
                }
            }
        }
    }
}

fn gt_const(
    ctx: &Context,
    ty: TypeId,
    value: u32,
    to_obj: fn(u32) -> Object,
) -> Option<impl Iterator<Item = Step>> {
    let mut steps = TVec::<StepId, Step>::new();
    if value == 0 {
        steps.push(Step::new(
            ctx.bool_ty,
            Action::LoadConstant(ctx.bool_to_object(false)),
        ));
    } else if to_obj(value).is_max_int() {
        let max = steps.push(Step::new(ty, Action::LoadConstant(to_obj(value))));
        steps.push(Step::new(
            ctx.bool_ty,
            Action::Binop(Binop::Neq, max, StepId::replacement(0)),
        ));
    } else if value.is_power_of_two() {
        let ct = steps.push(Step::new(ty, Action::LoadConstant(to_obj(!(value - 1)))));
        let and = steps.push(Step::new(
            ty,
            Action::Binop(Binop::BitAnd, StepId::replacement(0), ct),
        ));
        let zero = steps.push(Step::new(ty, Action::LoadConstant(to_obj(0))));
        steps.push(Step::new(ctx.bool_ty, Action::Binop(Binop::Eq, and, zero)));
    } else {
        return None;
    }
    Some(steps.into_iter())
}

fn lt_const(
    ctx: &Context,
    ty: TypeId,
    value: u32,
    to_obj: fn(u32) -> Object,
) -> Option<impl Iterator<Item = Step>> {
    let mut steps = TVec::<StepId, Step>::new();
    if to_obj(value).is_max_int() {
        steps.push(Step::new(
            ctx.bool_ty,
            Action::LoadConstant(ctx.bool_to_object(false)),
        ));
    } else if value == 0 {
        let zero = steps.push(Step::new(ty, Action::LoadConstant(to_obj(0))));
        steps.push(Step::new(
            ctx.bool_ty,
            Action::Binop(Binop::Neq, zero, StepId::replacement(0)),
        ));
    } else if (value + 1).is_power_of_two() {
        let ct = steps.push(Step::new(ty, Action::LoadConstant(to_obj(!value))));
        let and = steps.push(Step::new(
            ty,
            Action::Binop(Binop::BitAnd, StepId::replacement(0), ct),
        ));
        let zero = steps.push(Step::new(ty, Action::LoadConstant(to_obj(0))));
        steps.push(Step::new(ctx.bool_ty, Action::Binop(Binop::Neq, and, zero)));
    } else {
        return None;
    }
    Some(steps.into_iter())
}

fn lte_const(
    ctx: &Context,
    ty: TypeId,
    value: u32,
    to_obj: fn(u32) -> Object,
) -> Option<impl Iterator<Item = Step>> {
    let mut steps = TVec::<StepId, Step>::new();
    if value == 0 {
        steps.push(Step::new(
            ctx.bool_ty,
            Action::LoadConstant(ctx.bool_to_object(true)),
        ));
    } else if to_obj(value).is_max_int() {
        let max = steps.push(Step::new(ty, Action::LoadConstant(to_obj(value))));
        steps.push(Step::new(
            ctx.bool_ty,
            Action::Binop(Binop::Eq, max, StepId::replacement(0)),
        ));
    } else if value.is_power_of_two() {
        let ct = steps.push(Step::new(ty, Action::LoadConstant(to_obj(!(value - 1)))));
        let and = steps.push(Step::new(
            ty,
            Action::Binop(Binop::BitAnd, StepId::replacement(0), ct),
        ));
        let zero = steps.push(Step::new(ty, Action::LoadConstant(to_obj(0))));
        steps.push(Step::new(ctx.bool_ty, Action::Binop(Binop::Neq, and, zero)));
    } else {
        return None;
    }
    Some(steps.into_iter())
}

fn multiply_const(
    ty: TypeId,
    value: u32,
    to_obj: fn(u32) -> Object,
) -> Option<impl Iterator<Item = Step>> {
    let mut steps = TVec::<StepId, Step>::new();
    if value.is_power_of_two() {
        let ct = steps.push(Step::new(
            ty,
            Action::LoadConstant(to_obj(value.trailing_zeros())),
        ));
        steps.push(Step::new(
            ty,
            Action::Binop(Binop::Shl, StepId::replacement(0), ct),
        ));
    } else {
        return None;
    }
    Some(steps.into_iter())
}

fn div_const(
    ty: TypeId,
    value: u32,
    to_obj: fn(u32) -> Object,
) -> Option<impl Iterator<Item = Step>> {
    let mut steps = TVec::<StepId, Step>::new();
    if value.is_power_of_two() {
        let ct = steps.push(Step::new(
            ty,
            Action::LoadConstant(to_obj(value.trailing_zeros())),
        ));
        steps.push(Step::new(
            ty,
            Action::Binop(Binop::Shr, StepId::replacement(0), ct),
        ));
    } else {
        return None;
    }
    Some(steps.into_iter())
}
