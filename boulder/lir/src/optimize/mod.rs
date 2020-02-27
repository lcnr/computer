use std::convert::identity;

use tindex::{bitset::TBitSet, tvec};

use graphc::{Coloring, Graph, NodeId};

use shared_id::LocationId;

use crate::{traits::UpdateLocation, Action, Block, Lir, Terminator};

impl<'a> Lir<'a> {
    /// Minimizes the needed memory of each block without
    /// modifying step order or visible behavior.
    pub fn minimize_memory_usage(&mut self) {
        for function in self.functions.iter_mut() {
            for block in function.blocks.iter_mut() {
                let coloring = block.calculate_coloring();

                block.update_locations(|location| {
                    LocationId(coloring.nodes[NodeId::from(location.0)])
                });

                block.memory_len = coloring.k;
            }
        }
    }

    /// Remove all moves where `o == i`.
    pub fn remove_noop_moves(&mut self) {
        for function in self.functions.iter_mut() {
            for block in function.blocks.iter_mut() {
                block.steps.retain(|s| {
                    if let Action::Move(o, i) = s {
                        o != i
                    } else {
                        true
                    }
                });
            }
        }
    }

    /// Removes `w_1` in the sequence `..., w_1, seq , w_2, ...`
    /// where `seq` does not read the given location.
    pub fn remove_dead_writes(&mut self) {
        for function in self.functions.iter_mut() {
            for block in function.blocks.iter_mut() {
                block.remove_dead_writes()
            }
        }
    }
}

impl Block {
    pub fn remove_dead_writes(&mut self) {
        let mut last_writes = tvec![None; self.memory_len];
        let mut to_remove = TBitSet::new();

        for step_id in self.steps.index_iter() {
            match self.steps[step_id] {
                Action::Invert(i, o) | Action::Move(i, o) => {
                    last_writes[i] = None;
                    if let Some(last) = last_writes[o].replace(step_id) {
                        to_remove.add(last);
                    }
                }
                Action::Debug(i) => last_writes[i] = None,
                Action::LoadInput(_, o) | Action::LoadConstant(_, o) => {
                    if let Some(last) = last_writes[o].replace(step_id) {
                        to_remove.add(last);
                    }
                }
                Action::Binop { l, r, out, .. } => {
                    last_writes[l] = None;
                    last_writes[r] = None;
                    if let Some(last) = last_writes[out].replace(step_id) {
                        to_remove.add(last);
                    }
                }
                Action::FunctionCall {
                    ref args, ref ret, ..
                } => {
                    for &arg in args {
                        last_writes[arg] = None;
                    }

                    for &v in ret.iter() {
                        // TODO: FunctionCall should ret should be an Option
                        if let Some(last) = last_writes[v].take() {
                            to_remove.add(last);
                        }
                    }
                }
            }
        }

        match self.terminator {
            Terminator::Goto(_, ref args) => {
                for &arg in args {
                    last_writes[arg] = None;
                }
            }
            Terminator::Match(expr, ref arms) => {
                for arm in arms {
                    for &arg in arm.args.iter() {
                        last_writes[arg] = None;
                    }
                }

                last_writes[expr] = None;
            }
        }

        for step in last_writes.iter().copied().filter_map(identity) {
            to_remove.add(step)
        }

        for step in to_remove.iter().rev() {
            self.steps.remove(step);
        }
    }

    pub fn calculate_coloring(&self) -> Coloring {
        let mut g = Graph::new();
        let mut alive = TBitSet::new();

        for _ in 0..self.memory_len {
            g.add_node();
        }

        let mut add_alive = |alive: &mut TBitSet<LocationId>, n: LocationId| {
            for other in alive.iter() {
                g.add_edge(NodeId::from(n.0), NodeId::from(other.0));
            }
            alive.add(n);
        };

        match self.terminator {
            Terminator::Goto(_, ref args) => {
                for &arg in args {
                    add_alive(&mut alive, arg);
                }
            }
            Terminator::Match(expr, ref arms) => {
                for arm in arms {
                    for &arg in arm.args.iter() {
                        add_alive(&mut alive, arg);
                    }
                }

                add_alive(&mut alive, expr);
            }
        }

        for step in self.steps.iter().rev() {
            match *step {
                Action::Invert(i, o) | Action::Move(i, o) => {
                    add_alive(&mut alive, o);
                    alive.remove(o);
                    add_alive(&mut alive, i);
                }
                Action::Debug(i) => add_alive(&mut alive, i),
                Action::LoadInput(_, o) | Action::LoadConstant(_, o) => {
                    add_alive(&mut alive, o);
                    alive.remove(o)
                }
                Action::Binop { l, r, out, .. } => {
                    add_alive(&mut alive, out);
                    alive.remove(out);
                    add_alive(&mut alive, l);
                    add_alive(&mut alive, r);
                }
                Action::FunctionCall {
                    ref args, ref ret, ..
                } => {
                    for &v in ret.iter().rev() {
                        add_alive(&mut alive, v);
                        alive.remove(v);
                    }

                    for &arg in args {
                        add_alive(&mut alive, arg);
                    }
                }
            }
        }

        g.minimal_coloring()
    }
}
