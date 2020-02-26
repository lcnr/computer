use std::{convert::TryInto, iter, mem};

use tindex::{bitset::TBitSet, TSlice, TVec};

use shared_id::{StepId, TypeId, BOOL_TYPE_ID, FALSE_TYPE_ID, TRUE_TYPE_ID, U8_TYPE_ID};

use crate::{
    binop::Binop, traits::UpdateTypeIds, Action, Function, MatchArm, Mir, Object, Step, Terminator,
    Type, UnaryOperation,
};

impl<'a> Mir<'a> {
    /// Replace enums with bytes,
    ///
    /// Replace `Terminator::Match` with `Terminator::MatchByte`.
    ///
    /// Requires all sum types to be reduced using `Mir::reduce_sum_types`.
    pub fn enum_to_byte(&mut self) {
        let mut used = TBitSet::new();
        for ty in self.types.iter() {
            if let Type::Sum(kinds) = ty {
                used.extend(kinds.iter());
            }
        }

        let mut replacements: TVec<TypeId, u8> = iter::repeat(u8::max_value())
            .take(self.types.len())
            .collect();
        for (i, idx) in used.iter().enumerate() {
            replacements[idx] = i.try_into().expect("enums require more than 256 values");
        }

        if replacements[TRUE_TYPE_ID] < replacements[FALSE_TYPE_ID] {
            replacements.swap(TRUE_TYPE_ID, FALSE_TYPE_ID);
        }

        self.ctx.true_replacement = replacements[TRUE_TYPE_ID];
        self.ctx.false_replacement = replacements[FALSE_TYPE_ID];

        for function in self.functions.iter_mut() {
            function.enum_to_byte(&self.types, &replacements);
        }

        // TODO: do not clone `self.types`
        let types = self.types.clone();

        for ty in self.types.iter_mut() {
            ty.update_type_ids(&mut |id| match types[*id] {
                Type::Sum(_) | Type::Unit => *id = U8_TYPE_ID,
                _ => (),
            });
        }
    }
}

impl<'a> Function<'a> {
    pub fn enum_to_byte(
        &mut self,
        types: &TSlice<TypeId, Type>,
        replacements: &TSlice<TypeId, u8>,
    ) {
        for block in self.blocks.iter_mut() {
            for step_id in (0..block.steps.len()).map(StepId).rev() {
                let step = &mut block.steps[step_id];
                match step.action {
                    Action::LoadConstant(ref mut obj) => {
                        obj.enum_to_byte(step.ty, types, replacements)
                    }
                    Action::UnaryOperation(UnaryOperation::Invert, id) => {
                        if step.ty == BOOL_TYPE_ID {
                            let mut steps = TVec::new();
                            let one = steps
                                .push(Step::new(U8_TYPE_ID, Action::LoadConstant(Object::U8(1))));
                            steps.push(Step::new(
                                U8_TYPE_ID,
                                Action::Binop(Binop::BitXor, StepId::replacement(0), one),
                            ));

                            block.insert_steps(step_id..=step_id, steps, iter::once(id));
                        }
                    }
                    Action::Extend(id) | Action::Reduce(id) => {
                        block.replace_step_with_existing(step_id, id)
                    }
                    _ => (),
                }
            }

            let terminator = mem::replace(&mut block.terminator, Terminator::invalid());
            if let Terminator::Match(step, old_arms) = terminator {
                let mut arms = Vec::new();
                for arm in old_arms {
                    if let Type::Sum(ref kinds) = types[arm.pat] {
                        for ty in kinds.iter() {
                            arms.push(MatchArm {
                                pat: replacements[ty],
                                target: arm.target,
                                args: arm.args.clone(),
                            });
                        }
                    } else {
                        arms.push(MatchArm {
                            pat: replacements[arm.pat],
                            target: arm.target,
                            args: arm.args,
                        });
                    }
                }

                block.terminator = Terminator::MatchByte(step, arms);
            } else {
                block.terminator = terminator;
            }
        }

        self.update_type_ids(&mut |id| match types[*id] {
            Type::Sum(_) | Type::Unit => *id = U8_TYPE_ID,
            _ => (),
        });
    }
}

impl Object {
    fn enum_to_byte(
        &mut self,
        ty: TypeId,
        types: &TSlice<TypeId, Type>,
        replacements: &TSlice<TypeId, u8>,
    ) {
        match self {
            Object::Unit => {
                *self = Object::U8(replacements[ty]);
            }
            Object::U8(_) | Object::Undefined | Object::U16(_) | Object::U32(_) => (),
            Object::Struct(content) => content
                .iter_mut()
                .zip(types[ty].expect_struct().iter())
                .for_each(|(obj, &ty)| obj.enum_to_byte(ty, types, replacements)),
            &mut Object::Variant(v, ref obj) => {
                assert_eq!(**obj, Object::Unit);
                *self = Object::U8(replacements[v]);
            }
            &mut Object::Field(field, ref mut obj) => obj.enum_to_byte(field, types, replacements),
        }
    }
}
