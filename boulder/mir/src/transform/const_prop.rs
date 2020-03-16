use shared_id::{FALSE_TYPE_ID, TRUE_TYPE_ID};

use crate::{binop::Binop, Action, Mir, Object, Type, UnaryOperation};

impl<'a> Mir<'a> {
    pub fn const_propagate(&mut self) {
        let ctx = &self.ctx;
        for func in self.functions.iter_mut() {
            for block in func.blocks.iter_mut() {
                for s in block.steps.index_iter() {
                    let step = &block.steps[s];
                    match block.steps[s].action {
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
                                (Binop::BitAnd, Some(obj @ Object::U8(0)), _)
                                | (Binop::BitAnd, Some(obj @ Object::U16(0)), _)
                                | (Binop::BitAnd, Some(obj @ Object::U32(0)), _)
                                | (Binop::BitAnd, _, Some(obj @ Object::U8(0)))
                                | (Binop::BitAnd, _, Some(obj @ Object::U16(0)))
                                | (Binop::BitAnd, _, Some(obj @ Object::U32(0))) => {
                                    block.steps[s].action = Action::LoadConstant(obj);
                                }
                                (Binop::Gte, _, Some(Object::U8(0)))
                                | (Binop::Gte, _, Some(Object::U16(0)))
                                | (Binop::Gte, _, Some(Object::U32(0)))
                                | (Binop::BitOr, _, Some(Object::Variant(TRUE_TYPE_ID, _)))
                                | (Binop::BitOr, Some(Object::Variant(TRUE_TYPE_ID, _)), _) => {
                                    block.steps[s].action =
                                        Action::LoadConstant(ctx.bool_to_object(true))
                                }
                                (Binop::Gt, Some(Object::U8(0)), _)
                                | (Binop::Gt, Some(Object::U16(0)), _)
                                | (Binop::Gt, Some(Object::U32(0)), _) => {
                                    block.steps[s].action =
                                        Action::LoadConstant(ctx.bool_to_object(false))
                                }
                                _ => (),
                            }
                        }
                    }
                }
            }
        }
    }
}
