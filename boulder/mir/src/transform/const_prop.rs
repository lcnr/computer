use crate::{Action, Mir, Object, Type};

impl<'a> Mir<'a> {
    pub fn const_propagate(&mut self) {
        for func in self.functions.iter_mut() {
            for block in func.blocks.iter_mut() {
                for s in block.steps.index_iter() {
                    let step = &block.steps[s];
                    match block.steps[s].action {
                        Action::LoadInput(_) | Action::LoadConstant(_) => {}
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
                        _ => {} /*
                                    InitializeStruct(TVec<FieldId, shared_id::StepId>),
                                    InitializeUnion(shared_id::StepId),
                                    CallFunction(FunctionId, Vec<shared_id::StepId>),
                                    StructFieldAccess(shared_id::StepId, FieldId),
                                    UnionFieldAccess(shared_id::StepId),
                                    UnaryOperation(UnaryOperation, shared_id::StepId),
                                    Binop(Binop, shared_id::StepId, shared_id::StepId),
                                */
                    }
                }
            }
        }
    }
}
