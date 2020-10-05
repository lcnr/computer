use crate::reachability::Reachability;
use crate::traits::{Reads, Update, Writes};
use crate::{Function, Lir};
use graphc::{Coloring, Graph, NodeId};
use shared_id::LocationId;
use std::{cmp, collections::HashMap};
use tindex::TBitSet;

impl<'a> Lir<'a> {
    pub fn minimize_memory_len(&mut self) {
        for func in self.functions.iter_mut() {
            func.minimize_memory_len();
        }

        self.remove_identity_moves();
    }
}

impl<'a> Function<'a> {
    fn minimize_memory_len(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("minimize_memory_len");
        let coloring = self.coloring();
        // Inputs and outputs are always in distinct colorings.
        let mut mapping: HashMap<usize, LocationId> = HashMap::new();
        let mut curr = LocationId(0);
        for i in 0..self.memory_len {
            mapping
                .entry(coloring.nodes[NodeId::from(i)])
                .or_insert_with(|| {
                    let c = curr;
                    curr = curr + 1;
                    c
                });
        }
        for block in self.blocks.iter_mut() {
            block.update(|i: LocationId| mapping[&coloring.nodes[NodeId::from(i.0)]]);
        }
        self.memory_len = coloring.k;
    }

    fn coloring(&self) -> Coloring {
        let mut g = Graph::new();
        for _ in 0..self.memory_len {
            g.add_node();
        }

        for i in 0..cmp::max(self.input_len, self.return_len) {
            for j in 0..cmp::max(self.input_len, self.return_len) {
                g.add_edge(NodeId::from(i), NodeId::from(j));
            }
        }

        for blk in self.blocks.index_iter() {
            let mut alive = TBitSet::new();
            fn add_alive(g: &mut Graph, alive: &mut TBitSet<LocationId>, n: LocationId) {
                for other in alive.iter() {
                    g.add_edge(NodeId::from(n.0), NodeId::from(other.0));
                }
                alive.add(n);
            }

            let reachability = self.terminator_reachability(blk);
            for l in reachability.index_iter() {
                if reachability[l] == Reachability::Access {
                    add_alive(&mut g, &mut alive, l);
                }
            }

            self.blocks[blk]
                .terminator
                .reads(|id| add_alive(&mut g, &mut alive, id));

            for step in self.blocks[blk].steps.iter().rev() {
                step.writes(|id| {
                    add_alive(&mut g, &mut alive, id);
                    alive.remove(id)
                });
                step.reads(|id| add_alive(&mut g, &mut alive, id));
            }
        }

        g.minimal_coloring()
    }
}
