use tindex::bitset::TBitSet;

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
}

impl Block {
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
                Action::Binop { op: _, l, r, out } => {
                    add_alive(&mut alive, out);
                    alive.remove(out);
                    add_alive(&mut alive, l);
                    add_alive(&mut alive, r);
                }
                Action::FunctionCall {
                    id: _,
                    ref args,
                    ref ret,
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
