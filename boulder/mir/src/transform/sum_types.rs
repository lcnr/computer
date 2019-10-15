use std::mem;

use tindex::{TSlice, TVec};

use shared_id::{FieldId, TypeId};

use crate::{
    traits::UpdateStepIds, Action, BlockId, Function, Mir, Object, Step, StepId, Terminator, Type,
};

impl<'a> Mir<'a> {
    /// split all sum types into a union and a tag
    pub fn reduce_sum_types(&mut self) {
        let mut tags = Vec::new();
        let mut sums = Vec::new();

        let mut replacements = self
            .types
            .index_iter()
            .map(|id| {
                if let &Type::Sum(ref cases) = &self.types[id] {
                    if cases.iter().all(|f| self.types[f] == Type::Unit) {
                        None
                    } else {
                        let tag_count = cases.element_count();
                        let sum_count = tag_count - 1;
                        let union_type = Type::Union(cases.iter().collect());
                        if tags.len() < tag_count {
                            tags.resize_with(tag_count, || self.types.push(Type::Unit));
                            while sums.len() < sum_count {
                                sums.push(self.types.push(Type::Sum(
                                    tags[0..sums.len() + 2].iter().copied().collect(),
                                )));
                            }
                        }
                        let tag = sums[sum_count - 1];
                        let un = self.types.push(union_type);
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
                Type::Uninhabited | Type::Unit | Type::U8 | Type::U16 | Type::U32 => (),
                &mut Type::Struct(ref mut fields) | &mut Type::Union(ref mut fields) => {
                    for field in fields.iter_mut() {
                        *field = replacements[*field].unwrap_or(*field);
                    }
                }
                &mut Type::Sum(ref mut cases) => {
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
        tags: &[TypeId],
        replacements: &TSlice<TypeId, Option<TypeId>>,
    ) {
        self.ret = replacements[self.ret].unwrap_or(self.ret);

        let mut block_id = BlockId::from(0);
        while block_id.0 < self.blocks.len() {
            let block = &mut self.blocks[block_id];
            let terminator = mem::replace(&mut block.terminator, Terminator::invalid());
            if let Terminator::Match(mut step, mut arms) = terminator {
                if let Some(replacement_ty) = replacements[block.steps[step].ty] {
                    let union_ty = types[replacement_ty].fields().last().copied().unwrap();
                    let sum_ty = types[replacement_ty].fields().first().copied().unwrap();

                    let union_step = block.steps.push(Step::new(
                        union_ty,
                        Action::StructFieldAccess(step, FieldId::from(1)),
                    ));
                    step = block.steps.push(Step::new(
                        sum_ty,
                        Action::StructFieldAccess(step, FieldId::from(0)),
                    ));

                    for (ty, target, steps) in arms.iter_mut() {
                        let old_ty = *ty;
                        if let Type::Sum(_) = &types[*ty] {
                            unimplemented!("match arm sum");
                        }

                        let nth = types[union_ty]
                            .fields()
                            .iter()
                            .position(|&f| f == *ty)
                            .unwrap();

                        *ty = types[sum_ty].expect_sum().iter().nth(nth).unwrap();

                        let mut self_steps = Vec::new();
                        for (i, step) in steps.iter_mut().enumerate() {
                            if *step == None {
                                self_steps.push(StepId::from(i));
                                *step = Some(union_step);
                            }
                        }

                        if !self_steps.is_empty() {
                            let block = self.add_block();
                            self.blocks[block].terminator = Terminator::Goto(
                                *target,
                                (0..steps.len()).map(StepId::from).collect(),
                            );
                            *target = Some(block);
                            for step in steps.iter() {
                                let ty = self.blocks[block_id].steps[step.unwrap()].ty;
                                self.blocks[block].add_input(ty);
                            }

                            for step in self_steps.into_iter() {
                                let id = self.blocks[block].add_step(
                                    old_ty,
                                    Action::UnionFieldAccess(step, FieldId::from(nth)),
                                );
                                self.blocks[block].terminator.replace_step(step, id);
                            }
                        }
                    }
                }
                self.blocks[block_id].terminator = Terminator::Match(step, arms);
            } else {
                self.blocks[block_id].terminator = terminator;
            }

            for input in self.blocks[block_id].input.iter_mut() {
                *input = replacements[*input].unwrap_or(*input);
            }

            let mut step_id = StepId::from(0);
            while step_id.0 < self.blocks[block_id].steps.len() {
                if let Some(ty) = replacements[self.blocks[block_id][step_id].ty] {
                    match &self.blocks[block_id][step_id].action {
                        &Action::LoadInput(_)
                        | &Action::CallFunction(_, _)
                        | &Action::StructFieldAccess(_, _) => {
                            self.blocks[block_id][step_id].ty = ty;
                        }
                        &Action::Extend(target) => {
                            let target_ty = self.blocks[block_id][target].ty;
                            let union_ty = types[ty].fields().last().copied().unwrap();
                            let sum_ty = types[ty].fields().first().copied().unwrap();
                            if let Type::Sum(_) = types[target_ty] {
                                unimplemented!("extend sum -> sum");
                            } else if replacements.contains(&Some(target_ty)) {
                                let target_union_ty =
                                    types[target_ty].fields().last().copied().unwrap();
                                let target_sum_ty =
                                    types[target_ty].fields().first().copied().unwrap();

                                let target_sum = step_id;
                                step_id = self.blocks[block_id].insert_step(
                                    step_id,
                                    Step::new(
                                        target_sum_ty,
                                        Action::StructFieldAccess(target, FieldId::from(0)),
                                    ),
                                );

                                let extended_sum = step_id;
                                step_id = self.blocks[block_id].insert_step(
                                    step_id,
                                    Step::new(sum_ty, Action::Extend(target_sum)),
                                );

                                let arr =
                                    [union_ty.min(target_union_ty), union_ty.max(target_union_ty)];
                                let union_union_ty = if let Some(ty) = types
                                    .iter()
                                    .position(|ty| ty.is_union() && ty.fields().to_slice() == &arr)
                                {
                                    TypeId::from(ty)
                                } else {
                                    let arr: &[TypeId] = &arr;
                                    types.push(Type::Union(TVec::from(arr)))
                                };

                                let target_union = step_id;
                                step_id = self.blocks[block_id].insert_step(
                                    step_id,
                                    Step::new(
                                        target_union_ty,
                                        Action::StructFieldAccess(target, FieldId::from(1)),
                                    ),
                                );

                                let union_union = step_id;
                                step_id = self.blocks[block_id].insert_step(
                                    step_id,
                                    Step::new(
                                        union_union_ty,
                                        Action::InitializeUnion(
                                            target_union,
                                            arr.iter()
                                                .position(|&t| t == target_union_ty)
                                                .map(FieldId::from)
                                                .unwrap(),
                                        ),
                                    ),
                                );

                                let extended_union = step_id;
                                step_id = self.blocks[block_id].insert_step(
                                    step_id,
                                    Step::new(
                                        union_ty,
                                        Action::UnionFieldAccess(
                                            union_union,
                                            arr.iter()
                                                .position(|&t| t == union_ty)
                                                .map(FieldId::from)
                                                .unwrap(),
                                        ),
                                    ),
                                );

                                self.blocks[block_id].steps[step_id] = Step::new(
                                    ty,
                                    Action::InitializeStruct(tvec![extended_sum, extended_union]),
                                );
                            } else {
                                let position = types[union_ty]
                                    .fields()
                                    .iter()
                                    .position(|&f| f == target_ty)
                                    .unwrap();
                                self.blocks[block_id].insert_step(
                                    step_id,
                                    Step::new(
                                        union_ty,
                                        Action::InitializeUnion(target, FieldId::from(position)),
                                    ),
                                );
                                self.blocks[block_id].insert_step(
                                    step_id,
                                    Step::new(
                                        sum_ty,
                                        Action::LoadConstant(Object::Variant(
                                            tags[position],
                                            Box::new(Object::Unit),
                                        )),
                                    ),
                                );
                                step_id.0 += 2;
                                self.blocks[block_id].insert_step(
                                    step_id,
                                    Step::new(
                                        ty,
                                        Action::InitializeStruct(tvec![
                                            StepId(step_id.0 - 2),
                                            StepId(step_id.0 - 1)
                                        ]),
                                    ),
                                );

                                self.blocks[block_id].replace_step(StepId(step_id.0 + 1), step_id);
                                //println!("{:?} ... {:?}", self.blocks[block_id][StepId(step_id.0 + 1)], self.blocks[block_id][step_id]);
                            }
                        }
                        action => {
                            unimplemented!("what else can be done with sum types: {:?}", action)
                        }
                    }
                }
                step_id.0 += 1;
            }
            block_id.0 += 1;
        }
    }
}
