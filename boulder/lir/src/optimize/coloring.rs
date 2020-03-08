use graphc::{Coloring, Graph, NodeId};

use tindex::{TBitSet, TVec};

use shared_id::LocationId;

use crate::{
    traits::{Reads, Update, Writes},
    Block, Lir,
};

impl<'a> Lir<'a> {
    /// Minimizes the needed memory of each block without
    /// modifying step order or visible behavior.
    ///
    /// TODO: this function is currently unable to reduce the
    /// memory size to 0, consider checking graphs with only 1 color
    /// manually
    pub fn minimize_memory_usage(&mut self) {
        #[cfg(feature = "profiler")]
        profile_scope!("minimize_memory_usage");

        for function in self.functions.iter_mut() {
            #[cfg(feature = "profiler")]
            profile_scope!("minimize_memory_usage::function");
            for block in function.blocks.iter_mut() {
                let coloring = block.coloring();

                for input in block.inputs.iter_mut() {
                    *input = LocationId(coloring.nodes[NodeId::from(input.0)]);
                }

                block.steps.iter_mut().for_each(|s| {
                    s.update(|location| LocationId(coloring.nodes[NodeId::from(location.0)]))
                });

                block.terminator.update(|location: LocationId| {
                    LocationId(coloring.nodes[NodeId::from(location.0)])
                });

                block.memory_len = coloring.k;
            }
        }
    }
}

impl Block {
    pub fn coloring(&mut self) -> Coloring {
        #[cfg(feature = "profiler")]
        profile_scope!("block_coloring");

        let mut g = Graph::new();
        let mut alive = TBitSet::new();

        // lir reuses locations, this makes the graph algorithm less efficent,
        // to solve this, we make each living location unique
        let mut replacements: TVec<LocationId, LocationId> =
            (0..).map(LocationId).take(self.memory_len).collect();
        let mut repl = (self.memory_len..).map(LocationId);

        for _ in 0..self.memory_len {
            g.add_node();
        }

        fn add_alive(g: &mut Graph, alive: &mut TBitSet<LocationId>, n: LocationId) {
            for other in alive.iter() {
                g.add_edge(NodeId::from(n.0), NodeId::from(other.0));
            }
            alive.add(n);
        }

        self.terminator
            .reads(|id| add_alive(&mut g, &mut alive, id));

        for step in self.steps.iter_mut().rev() {
            step.writes(|id| {
                let updated = replacements[id];
                add_alive(&mut g, &mut alive, updated);
                alive.remove(updated);

                g.add_node();
                replacements[id] = repl.next().unwrap();

                updated
            });
            step.reads(|id| {
                let updated = replacements[id];
                add_alive(&mut g, &mut alive, updated);
                updated
            });
        }

        for id in self.inputs.iter_mut().rev() {
            let updated = replacements[*id];
            add_alive(&mut g, &mut alive, updated);
            alive.remove(updated);

            g.add_node();
            replacements[*id] = repl.next().unwrap();

            *id = updated;
        }

        g.minimal_coloring()
    }
}
