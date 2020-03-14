use std::{iter, mem};

use tindex::{TSlice, TVec};

use shared_id::{FieldId, StepId, TypeId};

use crate::{traits::UpdateStepIds, Action, Function, Mir, Object, Step, Terminator, Type};

impl<'a> Mir<'a> {
    /// split all sum types into a union and a tag
    pub fn reduce_sum_types(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("Mir::reduce_sum_types");
        let tags: TVec<TypeId, TypeId> = self
            .types
            .index_iter()
            .map(|idx| {
                if let Type::Unit = self.types[idx] {
                    idx
                } else {
                    self.types.push(Type::Unit)
                }
            })
            .collect();

        let mut replacements = self
            .types
            .index_iter()
            .map(|id| {
                if let Type::Sum(ref cases) = self.types[id] {
                    if cases.iter().all(|f| self.types[f] == Type::Unit) {
                        None
                    } else {
                        let un_ty = Type::Union(cases.iter().collect());
                        let tag_ty = Type::Sum(cases.iter().map(|c| tags[c]).collect());
                        let un = self.types.push(un_ty);
                        let tag = self.types.push(tag_ty);
                        Some(self.types.push(Type::Struct(tvec![tag, un])))
                    }
                } else {
                    None
                }
            })
            .collect::<TVec<TypeId, _>>();

        replacements.resize(self.types.len(), None);

        for ty in self.types.iter_mut() {
            match ty {
                Type::Uninhabited | Type::Unit | Type::U8 | Type::U16 | Type::U32 => {}
                &mut Type::Struct(ref mut fields) => {
                    for field in fields.iter_mut() {
                        *field = replacements[*field].unwrap_or(*field);
                    }
                }
                &mut Type::Sum(ref mut cases) | &mut Type::Union(ref mut cases) => {
                    for case in cases.clone().into_iter() {
                        if let Some(replacement) = replacements[case] {
                            cases.remove(case);
                            cases.add(replacement);
                        }
                    }
                }
            }
        }

        for func in self.functions.iter_mut() {
            func.reduce_sum_types(&mut self.types, &tags, &replacements);
        }
    }
}

impl<'a> Function<'a> {
    pub fn reduce_sum_types(
        &mut self,
        types: &mut TVec<TypeId, Type>,
        tags: &TSlice<TypeId, TypeId>,
        replacements: &TSlice<TypeId, Option<TypeId>>,
    ) {
        #[cfg(feature = "profiler")]
        profile_scope!("Function::reduce_sum_types");
        self.ret = replacements[self.ret].unwrap_or(self.ret);

        for block_id in self.blocks.index_iter() {
            let block = &mut self.blocks[block_id];
            let terminator = mem::replace(&mut block.terminator, Terminator::invalid());
            if let Terminator::Match(mut step, mut arms) = terminator {
                if let Some(replacement_ty) = replacements[block.steps[step].ty] {
                    let (&union_ty, &sum_ty) = types[replacement_ty]
                        .expect_struct()
                        .split_last()
                        .map(|(last, rest)| (last, rest.first().unwrap()))
                        .unwrap();

                    let union_step = block.steps.push(Step::new(
                        union_ty,
                        Action::StructFieldAccess(step, FieldId::from(1)),
                    ));
                    step = block.steps.push(Step::new(
                        sum_ty,
                        Action::StructFieldAccess(step, FieldId::from(0)),
                    ));

                    for arm in arms.iter_mut() {
                        let old_ty = arm.pat;
                        if let Type::Sum(_) = types[arm.pat] {
                            if let Some(arm_replacement_ty) = replacements[arm.pat] {
                                let &arm_sum_ty =
                                    types[arm_replacement_ty].expect_struct().first().unwrap();
                                arm.pat = arm_sum_ty;

                                let mut self_steps = Vec::new();
                                for (i, step) in arm.args.iter_mut().enumerate() {
                                    if *step == None {
                                        self_steps.push(StepId(i));
                                        *step = Some(union_step);
                                    }
                                }

                                if !self_steps.is_empty() {
                                    arm.args.push(None);
                                    let block = self.add_block();
                                    self.blocks[block].terminator = Terminator::Goto(
                                        arm.target,
                                        (0..arm.args.len()).map(StepId).collect(),
                                    );
                                    arm.target = Some(block);
                                    for step in arm.args.iter() {
                                        let ty = self.blocks[block_id].steps[step.unwrap()].ty;
                                        self.blocks[block].add_input(ty);
                                    }
                                    let block_sum_input = self.blocks[block].add_input(sum_ty);

                                    for step in self_steps.into_iter() {
                                        let target_union_ty = types[replacement_ty]
                                            .expect_struct()
                                            .last()
                                            .copied()
                                            .unwrap();

                                        let reduced_sum = self.blocks[block]
                                            .add_step(arm_sum_ty, Action::Reduce(block_sum_input));

                                        let union_union_ty = super::get_or_insert_union(
                                            types,
                                            iter::once(union_ty.min(target_union_ty))
                                                .chain(iter::once(union_ty.max(target_union_ty))),
                                        );

                                        let target_union = self.blocks[block_id].add_step(
                                            target_union_ty,
                                            Action::StructFieldAccess(step, FieldId::from(1)),
                                        );

                                        let union_union = self.blocks[block_id].add_step(
                                            union_union_ty,
                                            Action::InitializeUnion(target_union),
                                        );

                                        let reduced_union = self.blocks[block_id].add_step(
                                            union_ty,
                                            Action::UnionFieldAccess(union_union),
                                        );

                                        let union_struct = self.blocks[block_id].add_step(
                                            arm_replacement_ty,
                                            Action::InitializeStruct(tvec![
                                                reduced_sum,
                                                reduced_union,
                                            ]),
                                        );
                                        self.blocks[block]
                                            .terminator
                                            .replace_step(step, union_struct);
                                    }
                                }
                            } else {
                                // a unit sum type, we can just reduce the sum and ignore the union
                                let mut self_steps = Vec::new();
                                for (i, arm_step) in arm.args.iter_mut().enumerate() {
                                    if *arm_step == None {
                                        self_steps.push(StepId(i));
                                        *arm_step = Some(step);
                                    }
                                }

                                if !self_steps.is_empty() {
                                    let block = self.add_block();
                                    self.blocks[block].terminator = Terminator::Goto(
                                        arm.target,
                                        (0..arm.args.len()).map(StepId).collect(),
                                    );
                                    arm.target = Some(block);
                                    for step in arm.args.iter() {
                                        let ty = self.blocks[block_id].steps[step.unwrap()].ty;
                                        self.blocks[block].add_input(ty);
                                    }

                                    for step in self_steps.into_iter() {
                                        let reduced_sum = self.blocks[block]
                                            .add_step(arm.pat, Action::Reduce(step));

                                        self.blocks[block]
                                            .terminator
                                            .replace_step(step, reduced_sum);
                                    }
                                }
                            }
                        } else {
                            arm.pat = tags[old_ty];

                            let mut self_steps = Vec::new();
                            for (i, step) in arm.args.iter_mut().enumerate() {
                                if *step == None {
                                    self_steps.push(StepId(i));
                                    *step = Some(union_step);
                                }
                            }

                            if !self_steps.is_empty() {
                                let block = self.add_block();
                                self.blocks[block].terminator = Terminator::Goto(
                                    arm.target,
                                    (0..arm.args.len()).map(StepId).collect(),
                                );
                                arm.target = Some(block);
                                for step in arm.args.iter() {
                                    let ty = self.blocks[block_id].steps[step.unwrap()].ty;
                                    self.blocks[block].add_input(ty);
                                }

                                for step in self_steps.into_iter() {
                                    let id = self.blocks[block]
                                        .add_step(old_ty, Action::UnionFieldAccess(step));
                                    self.blocks[block].terminator.replace_step(step, id);
                                }
                            }
                        }
                    }
                }
                self.blocks[block_id].terminator = Terminator::Match(step, arms);
            } else {
                self.blocks[block_id].terminator = terminator;
            }

            for input in self.blocks[block_id].input.iter_mut() {
                if let Some(replacement) = replacements[*input] {
                    *input = replacement;
                }
            }

            for step_id in self.blocks[block_id].steps.index_iter().rev() {
                let step_ty = self.blocks[block_id].steps[step_id].ty;
                if let Some(ty) = replacements[step_ty] {
                    match self.blocks[block_id][step_id].action {
                        Action::LoadInput(_)
                        | Action::CallFunction(_, _)
                        | Action::StructFieldAccess(_, _) => {
                            self.blocks[block_id][step_id].ty = ty;
                        }
                        Action::Extend(target) => {
                            let new_steps = build_extend_steps(
                                types,
                                replacements,
                                tags,
                                self.blocks[block_id][target].ty,
                                ty,
                            );

                            self.blocks[block_id].insert_steps(
                                step_id..=step_id,
                                new_steps,
                                iter::once(target),
                            );
                        }
                        Action::LoadConstant(ref mut obj) => {
                            obj.reduce_sum_types(
                                types,
                                replacements,
                                tags,
                                replacements[step_ty].unwrap_or(step_ty),
                            );
                            self.blocks[block_id][step_id].ty = ty;
                        }
                        ref action => {
                            unreachable!("what else can be done with sum types: {:?}", action)
                        }
                    }
                } else if let Action::LoadConstant(ref mut obj) =
                    self.blocks[block_id][step_id].action
                {
                    obj.reduce_sum_types(types, replacements, tags, step_ty);
                }
            }
        }
    }
}

fn build_extend_steps(
    types: &mut TVec<TypeId, Type>,
    replacements: &TSlice<TypeId, Option<TypeId>>,
    tags: &TSlice<TypeId, TypeId>,
    target_ty: TypeId,
    ty: TypeId,
) -> TVec<StepId, Step> {
    if let Some(target_ty) = replacements[target_ty] {
        let mut new_steps = TVec::new();
        let (&union_ty, &sum_ty) = types[ty]
            .expect_struct()
            .split_last()
            .map(|(last, rest)| (last, rest.first().unwrap()))
            .unwrap();

        let (&target_union_ty, &target_sum_ty) = types[target_ty]
            .expect_struct()
            .split_last()
            .map(|(last, rest)| (last, rest.first().unwrap()))
            .unwrap();

        let target_sum = new_steps.push(Step::new(
            target_sum_ty,
            Action::StructFieldAccess(StepId::replacement(0), FieldId::from(0)),
        ));

        let extended_sum = new_steps.push(Step::new(sum_ty, Action::Extend(target_sum)));
        let union_union_ty = super::get_or_insert_union(
            types,
            [union_ty.min(target_union_ty), union_ty.max(target_union_ty)]
                .iter()
                .copied(),
        );
        let target_union = new_steps.push(Step::new(
            target_union_ty,
            Action::StructFieldAccess(StepId::replacement(0), FieldId::from(1)),
        ));
        let union_union = new_steps.push(Step::new(
            union_union_ty,
            Action::InitializeUnion(target_union),
        ));
        let extended_union =
            new_steps.push(Step::new(union_ty, Action::UnionFieldAccess(union_union)));
        new_steps.push(Step::new(
            ty,
            Action::InitializeStruct(tvec![extended_sum, extended_union]),
        ));
        new_steps
    } else if let Type::Sum(_) = types[target_ty] {
        let mut new_steps = TVec::new();

        let (&union_ty, &sum_ty) = types[ty]
            .expect_struct()
            .split_last()
            .map(|(last, rest)| (last, rest.first().unwrap()))
            .unwrap();
        let union_step =
            new_steps.push(Step::new(union_ty, Action::LoadConstant(Object::Undefined)));
        let sum_step = new_steps.push(Step::new(sum_ty, Action::Extend(StepId::replacement(0))));
        new_steps.push(Step::new(
            ty,
            Action::InitializeStruct(tvec![sum_step, union_step]),
        ));
        new_steps
    } else {
        let (&union_ty, &sum_ty) = types[ty]
            .expect_struct()
            .split_last()
            .map(|(last, rest)| (last, rest.first().unwrap()))
            .unwrap();

        let mut new_steps = TVec::new();
        let un = new_steps.push(Step::new(
            union_ty,
            Action::InitializeUnion(StepId::replacement(0)),
        ));
        let sum = new_steps.push(Step::new(
            sum_ty,
            Action::LoadConstant(Object::Variant(tags[target_ty], Box::new(Object::Unit))),
        ));
        new_steps.push(Step::new(ty, Action::InitializeStruct(tvec![sum, un])));

        new_steps
    }
}

impl Object {
    fn reduce_sum_types(
        &mut self,
        types: &mut TVec<TypeId, Type>,
        replacements: &TSlice<TypeId, Option<TypeId>>,
        tags: &TSlice<TypeId, TypeId>,
        ty: TypeId,
    ) {
        match *self {
            Object::U8(_) | Object::Undefined | Object::U16(_) | Object::U32(_) | Object::Unit => {}
            Object::Field(ty, ref mut inner) => {
                inner.reduce_sum_types(types, replacements, tags, ty)
            }
            Object::Struct(ref mut content) => {
                let struct_data = types[ty].expect_struct().to_owned();
                content
                    .iter_mut()
                    .zip(struct_data.iter())
                    .for_each(|(obj, &ty)| obj.reduce_sum_types(types, replacements, tags, ty))
            }
            Object::Variant(v, ref obj) => {
                if replacements.contains(&Some(ty)) {
                    if let Type::Unit = &types[v] {
                        assert_eq!(obj.as_ref(), &Object::Unit);

                        *self = Object::Struct(tvec![
                            Object::Variant(v, obj.clone()),
                            Object::Undefined
                        ]);
                    } else if replacements.contains(&Some(v)) {
                        unreachable!();
                    } else {
                        *self = Object::Struct(tvec![
                            Object::Variant(tags[v], Box::new(Object::Unit)),
                            Object::Field(v, obj.clone()),
                        ]);
                    }
                } else {
                    assert_eq!(obj.as_ref(), &Object::Unit,);
                }
            }
        }
    }
}
