use crate::{Action, Mir, Object};

impl<'a> Mir<'a> {
    pub fn const_propagate(&mut self) {
        for func in self.functions.iter_mut() {
            for block in func.blocks.iter_mut() {
                for s in block.steps.index_iter() {
                    match block.steps[s].action {
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
                        _ => (),
                        /*
                            Reduce(shared_id::StepId),
                            LoadInput(usize),
                            LoadConstant(Object),
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
