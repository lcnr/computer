use std::mem;

use shared_id::{FunctionId, BOOL_TYPE_ID, FALSE_TYPE_ID, TRUE_TYPE_ID};

use crate::{traits::UpdateStepIds, Action, BlockId, Mir, Object, Step, StepId, Terminator, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Shl,
    Shr,
    Eq,
    Lt,
    Gt,
    BitOr,
    BitAnd,
}

impl Binop {
    pub fn validate(&self, this: &Step, a: &Step, b: &Step) {
        match self {
            Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Shl
            | Self::Shr
            | Self::BitOr
            | Self::BitAnd => {
                assert_eq!(a.ty, b.ty);
                assert_eq!(a.ty, this.ty);
            }
            Self::Lt | Self::Eq | Self::Gt => {
                assert_eq!(a.ty, b.ty);
                assert_eq!(this.ty, BOOL_TYPE_ID);
            }
        }
    }
}

impl Mir {
    fn reduce_binops_in_block(&mut self, function: FunctionId, block: BlockId) {
        let function = &mut self.functions[function];

        let mut i = StepId(0);
        while i.0 < function[block].steps.len() {
            match &function[block].steps[i].action {
                &Action::Binop(Binop::Mul, a, b) => {
                    let op_ty = function[block].steps[i].ty;

                    function.split_block(block, i);
                    let (result, curr_bit, n_bits_action, const_int_fn) = match self.types[op_ty] {
                        Type::U8 => {
                            let result = function[block]
                                .add_step(op_ty, Action::LoadConstant(Object::U8(0)));
                            let curr_bit = function[block]
                                .add_step(op_ty, Action::LoadConstant(Object::U8(0)));
                            (
                                result,
                                curr_bit,
                                Action::LoadConstant(Object::U8(8)),
                                (&|a| Action::LoadConstant(Object::U8(a))) as &dyn Fn(u8) -> Action,
                            )
                        }
                        Type::U16 => {
                            let result = function[block]
                                .add_step(op_ty, Action::LoadConstant(Object::U16(0)));
                            let curr_bit = function[block]
                                .add_step(op_ty, Action::LoadConstant(Object::U16(0)));
                            (
                                result,
                                curr_bit,
                                Action::LoadConstant(Object::U16(16)),
                                (&|a: u8| Action::LoadConstant(Object::U16(a as u16)))
                                    as &dyn Fn(u8) -> Action,
                            )
                        }
                        Type::U32 => {
                            let result = function[block]
                                .add_step(op_ty, Action::LoadConstant(Object::U32(0)));
                            let curr_bit = function[block]
                                .add_step(op_ty, Action::LoadConstant(Object::U32(0)));
                            (
                                result,
                                curr_bit,
                                Action::LoadConstant(Object::U32(32)),
                                (&|a: u8| Action::LoadConstant(Object::U32(a as u32)))
                                    as &dyn Fn(u8) -> Action,
                            )
                        }
                        _ => unreachable!("non integer multiplication"),
                    };

                    function[block].replace_step(i, result);
                    let curr_bit = StepId(curr_bit.0 - 1);
                    let intro = function.split_block(block, curr_bit);
                    if let Terminator::Goto(_, ref mut steps) = function[block].terminator {
                        steps.push(curr_bit);
                        steps.push(a);
                        steps.push(b);
                    } else {
                        unreachable!("unexpected terminator after split_block");
                    }

                    let intro_block = &mut function[intro];
                    let terminal;
                    let intro_eq = {
                        let curr_bit = intro_block.add_input(op_ty);
                        let a = intro_block.add_input(op_ty);
                        let b = intro_block.add_input(op_ty);
                        let one = intro_block.add_step(op_ty, const_int_fn(1));
                        let shifted =
                            intro_block.add_step(op_ty, Action::Binop(Binop::Shl, one, curr_bit));
                        let and =
                            intro_block.add_step(op_ty, Action::Binop(Binop::BitAnd, b, shifted));
                        let zero = intro_block.add_step(op_ty, const_int_fn(0));
                        let check =
                            intro_block.add_step(BOOL_TYPE_ID, Action::Binop(Binop::Eq, and, zero));
                        let iiiiiiiiiii = intro_block.clone();
                        terminal = function.split_block(intro, check);
                        assert_eq!(iiiiiiiiiii.steps, function[intro].steps);
                        if let Terminator::Goto(_, ref mut steps) = function[intro].terminator {
                            steps.push(curr_bit);
                            steps.push(a);
                            steps.push(b);
                        } else {
                            unreachable!("unexpected terminator after split_block");
                        }
                        check
                    };
                    let curr_bit = function[terminal].add_input(op_ty);
                    // the step ids in `function.split_block` are ordered
                    let result = StepId(curr_bit.0 - 1);
                    let a = function[terminal].add_input(op_ty);
                    let b = function[terminal].add_input(op_ty);
                    {
                        let success = function.clone_block(terminal);
                        let shifted = function[success]
                            .add_step(op_ty, Action::Binop(Binop::Shl, a, curr_bit));
                        let add = function[success]
                            .add_step(op_ty, Action::Binop(Binop::Add, result, shifted));
                        function[success].terminator.replace_step(result, add);
                        if let Terminator::Goto(ref mut target, ref mut steps) =
                            function[success].terminator
                        {
                            *target = terminal;
                            steps.push(curr_bit);
                            steps.push(a);
                            steps.push(b);
                        } else {
                            unreachable!("unexpected terminator after split_block");
                        }

                        if let Terminator::Goto(terminal, steps) =
                            mem::replace(&mut function[intro].terminator, Terminator::invalid())
                        {
                            let steps: Vec<_> = steps.into_iter().map(Some).collect();
                            function[intro].terminator = Terminator::Match(
                                intro_eq,
                                vec![
                                    (TRUE_TYPE_ID, terminal, steps.clone()),
                                    (FALSE_TYPE_ID, success, steps),
                                ],
                            )
                        } else {
                            unreachable!("unexpected terminator after split_block");
                        }
                    }
                    let one = function[terminal].add_step(op_ty, const_int_fn(1));
                    let curr_bit = function[terminal]
                        .add_step(op_ty, Action::Binop(Binop::Add, curr_bit, one));
                    let n_bits = function[terminal].add_step(op_ty, n_bits_action);
                    let check = function[terminal]
                        .add_step(BOOL_TYPE_ID, Action::Binop(Binop::Lt, curr_bit, n_bits));
                    if let Terminator::Goto(fin, steps) =
                        mem::replace(&mut function[terminal].terminator, Terminator::invalid())
                    {
                        let steps: Vec<_> = steps.into_iter().map(Some).collect();
                        let mut loop_steps = steps.clone();
                        loop_steps.push(Some(curr_bit));
                        loop_steps.push(Some(a));
                        loop_steps.push(Some(b));
                        function[terminal].terminator = Terminator::Match(
                            check,
                            vec![
                                (TRUE_TYPE_ID, intro, loop_steps),
                                (FALSE_TYPE_ID, fin, steps),
                            ],
                        )
                    } else {
                        unreachable!("unexpected terminator after split_block");
                    }
                }
                _ => (),
            }

            i.0 += 1;
        }
    }

    pub fn reduce_binops(&mut self) {
        for function in 0..self.functions.len() {
            let function = FunctionId::from(function);
            let mut block = BlockId(0);
            while block.0 < self[function].blocks.len() {
                self.reduce_binops_in_block(function, block);
                block.0 += 1;
            }
        }
    }
}
