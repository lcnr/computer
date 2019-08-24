use std::{fmt, hash::Hash, ops::Index};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EntityId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProductionId(usize);

#[derive(Debug, Clone)]
enum Rule {
    ForwardProduction(ProductionId, EntityId, EntityId),
    BackwardsProduction(ProductionId, EntityId, EntityId),
}

#[derive(Debug)]
pub struct Solution<T> {
    inner: Vec<T>,
}

impl<T> Index<EntityId> for Solution<T> {
    type Output = T;

    fn index(&self, id: EntityId) -> &T {
        &self.inner[id.0]
    }
}

#[derive(Debug)]
pub enum SolveError<T, E> {
    UnsolvedEntities(Vec<(EntityId, Vec<T>)>),
    ProductionError(E),
}

pub struct Entity<'a, T> {
    pub id: EntityId,
    pub content: &'a mut Vec<T>,
}

pub trait Production<C, T, E>: fmt::Debug {
    fn resolve(&mut self, ctx: &mut C, origin: Entity<T>, target: Entity<T>) -> Result<(), E>;

    fn resolve_backwards(
        &mut self,
        ctx: &mut C,
        origin: Entity<T>,
        target: Entity<T>,
    ) -> Result<(), E>;
}

pub struct ConstraintSolver<C, T, E> {
    entities: Vec<Vec<T>>,
    rules: Vec<Vec<Rule>>,
    productions: Vec<Box<dyn Production<C, T, E>>>,
    context: C,
}

impl<'ctx, C: fmt::Debug, T: fmt::Debug, E> fmt::Debug for ConstraintSolver<C, T, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ConstraintSolver")
            .field("entities", &self.entities)
            .field("rules", &self.rules)
            .field("context", &self.context)
            .field("productions", &self.productions)
            .finish()
    }
}

impl<C: fmt::Debug, T: Eq + Hash + Clone + std::fmt::Debug, E> ConstraintSolver<C, T, E> {
    pub fn new(context: C) -> Self {
        Self {
            entities: Vec::new(),
            rules: Vec::new(),
            productions: Vec::new(),
            context,
        }
    }

    pub fn context(&mut self) -> &mut C {
        &mut self.context
    }

    /// # Panic
    ///
    /// This function panics if `values` is empty.
    pub fn add_entity(&mut self, values: Vec<T>) -> EntityId {
        assert!(
            !values.is_empty(),
            "Tried adding an entity without any valid values"
        );
        let id = EntityId(self.entities.len());
        self.entities.push(values);
        self.rules.push(Vec::new());
        id
    }

    pub fn define_production(&mut self, production: Box<dyn Production<C, T, E>>) -> ProductionId {
        let id = ProductionId(self.productions.len());
        self.productions.push(production);
        id
    }

    pub fn add_production(&mut self, production: ProductionId, origin: EntityId, target: EntityId) {
        assert_ne!(origin.0, target.0, "invalid production");
        self.rules[origin.0].push(Rule::ForwardProduction(production, origin, target));
        self.rules[target.0].push(Rule::BackwardsProduction(production, origin, target));
    }

    fn apply_entity(&mut self, id: usize) -> Result<(), E> {
        for rule in self.rules[id].iter() {
            match rule {
                &Rule::ForwardProduction(prod, actual_origin, target_id) => {
                    let (begin, end) = self.entities.split_at_mut(id.max(target_id.0));
                    let (origin, target) = if target_id.0 > id {
                        (&mut begin[id], &mut end[0])
                    } else {
                        (&mut end[0], &mut begin[target_id.0])
                    };
                    self.productions[prod.0].resolve(
                        &mut self.context,
                        Entity {
                            id: actual_origin,
                            content: origin,
                        },
                        Entity {
                            id: target_id,
                            content: target,
                        },
                    )?;
                }
                &Rule::BackwardsProduction(prod, origin_id, actual_target) => {
                    let (begin, end) = self.entities.split_at_mut(id.max(origin_id.0));
                    let (origin, target) = if id > origin_id.0 {
                        (&mut begin[origin_id.0], &mut end[0])
                    } else {
                        (&mut end[0], &mut begin[id])
                    };
                    self.productions[prod.0].resolve_backwards(
                        &mut self.context,
                        Entity {
                            id: origin_id,
                            content: origin,
                        },
                        Entity {
                            id: actual_target,
                            content: target,
                        },
                    )?;
                }
            }

            if self.entities[id].len() != 1 {
                break;
            }
        }

        Ok(())
    }

    /// finds a solution for the current set of constraints,
    /// returning an error in case there are either multiple or none.
    pub fn solve(&mut self) -> Result<Solution<T>, SolveError<T, E>> {
        if let Some((i, _)) = self.entities.iter().enumerate().find(|(_, e)| e.is_empty()) {
            panic!("Entity without possible state: {}", i);
        }

        let mut all_ids: Box<[usize]> = (0..self.entities.len()).map(|v| v).collect::<Box<_>>();
        let mut step_count = 0;
        let mut first = true;
        while first || all_ids.iter().any(|&id| self.entities[id].len() != 1) {
            first = false;
            let mut ids: &mut [usize] = &mut all_ids;
            while let Some(pos) = ids.iter().position(|&id| self.entities[id].len() == 1) {
                ids.swap(0, pos);
                self.apply_entity(ids[0])
                    .map_err(|e| SolveError::ProductionError(e))?;

                if self.entities[ids[0]].len() == 1 {
                    ids = &mut ids[1..];
                }
                step_count += 1;

                if step_count > 1000 {
                    return Err(SolveError::UnsolvedEntities(
                        all_ids
                            .iter()
                            .filter(|&&id| self.entities[id].len() != 1)
                            .map(|&id| (EntityId(id), self.entities[id].clone()))
                            .collect(),
                    ));
                }
            }
        }

        Ok(Solution {
            inner: self
                .entities
                .iter()
                .map(|e| e.last().unwrap().clone())
                .collect(),
        })
    }
}
