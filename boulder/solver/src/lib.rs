#[cfg(feature = "profiler")]
#[macro_use]
extern crate thread_profiler;

use std::fmt;

use tindex::{TIndex, TVec};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntityId(usize);

impl From<usize> for EntityId {
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl TIndex for EntityId {
    fn as_index(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProductionId(usize);

impl From<usize> for ProductionId {
    fn from(v: usize) -> Self {
        Self(v)
    }
}

impl TIndex for ProductionId {
    fn as_index(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
enum Rule {
    ForwardProduction(ProductionId, EntityId, EntityId),
    BackwardsProduction(ProductionId, EntityId, EntityId),
}

#[derive(Debug)]
pub enum SolveError<T, E> {
    UnsolvedEntities(Vec<(EntityId, T)>),
    ProductionError(E),
}

pub struct Entity<'a, T: EntityState> {
    pub id: EntityId,
    pub state: &'a mut T,
}

pub struct SolvedEntity<T: EntityState> {
    pub id: EntityId,
    pub value: T::Result,
}

pub trait EntityState {
    type Result;
    /// is the current entity uniquely identified
    fn solved(&self) -> bool;
    /// this function will only be called in case `entity.solved()` is true
    fn solution(&self) -> Self::Result;
}

pub trait Production<C, T: EntityState, E>: fmt::Debug {
    fn resolve(&mut self, ctx: &mut C, origin: SolvedEntity<T>, target: Entity<T>)
        -> Result<(), E>;

    fn resolve_backwards(
        &mut self,
        ctx: &mut C,
        origin: Entity<T>,
        target: SolvedEntity<T>,
    ) -> Result<(), E>;
}

pub struct ConstraintSolver<'a, C, T, E> {
    entities: TVec<EntityId, T>,
    rules: TVec<EntityId, Vec<Rule>>,
    productions: TVec<ProductionId, Box<dyn Production<C, T, E> + 'a>>,
    context: C,
}

impl<'ctx, 'a, C: fmt::Debug, T: fmt::Debug + EntityState, E> fmt::Debug
    for ConstraintSolver<'a, C, T, E>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ConstraintSolver")
            .field("entities", &self.entities)
            .field("rules", &self.rules)
            .field("context", &self.context)
            .field("productions", &self.productions)
            .finish()
    }
}

impl<'a, C: fmt::Debug, T: EntityState + Clone + std::fmt::Debug, E> ConstraintSolver<'a, C, T, E> {
    pub fn new(context: C) -> Self {
        Self {
            entities: TVec::new(),
            rules: TVec::new(),
            productions: TVec::new(),
            context,
        }
    }

    pub fn context(&mut self) -> &mut C {
        &mut self.context
    }

    pub fn add_entity(&mut self, values: T) -> EntityId {
        let id = self.entities.push(values);
        assert_eq!(self.rules.push(Vec::new()), id);
        id
    }

    pub fn override_entity_state(&mut self, entity: EntityId, state: T) {
        self.entities[entity] = state;
    }

    pub fn define_production(
        &mut self,
        production: Box<dyn Production<C, T, E> + 'a>,
    ) -> ProductionId {
        self.productions.push(production)
    }

    pub fn add_production(&mut self, production: ProductionId, origin: EntityId, target: EntityId) {
        assert_ne!(origin, target, "invalid production");
        self.rules[origin].push(Rule::ForwardProduction(production, origin, target));
        self.rules[target].push(Rule::BackwardsProduction(production, origin, target));
    }

    fn apply_entity(&mut self, id: EntityId) -> Result<(), E> {
        #[cfg(feature = "profiler")]
        profile_scope!("apply_entity");
        for rule in self.rules[id].iter() {
            match rule {
                &Rule::ForwardProduction(prod, actual_origin, target_id) => {
                    let (begin, end) = self.entities.split_at_mut(id.max(target_id));
                    let (origin, target) = if target_id > id {
                        (&mut begin[id], end.first_mut().unwrap())
                    } else {
                        (end.first_mut().unwrap(), &mut begin[target_id])
                    };
                    /* */// println!("before {:?}: {:?}, {:?}",  rule, origin, target);
                    self.productions[prod].resolve(
                        &mut self.context,
                        SolvedEntity {
                            id: actual_origin,
                            value: origin.solution(),
                        },
                        Entity {
                            id: target_id,
                            state: target,
                        },
                    )?;
                    /* */// println!("after {:?}: {:?}, {:?}\n",  rule, origin, target);
                }
                &Rule::BackwardsProduction(prod, origin_id, actual_target) => {
                    let (begin, end) = self.entities.split_at_mut(id.max(origin_id));
                    let (origin, target) = if id > origin_id {
                        (&mut begin[origin_id], end.first_mut().unwrap())
                    } else {
                        (end.first_mut().unwrap(), &mut begin[id])
                    };
                    /* */// println!("before {:?}: {:?}, {:?}",  rule, origin, target);
                    self.productions[prod].resolve_backwards(
                        &mut self.context,
                        Entity {
                            id: origin_id,
                            state: origin,
                        },
                        SolvedEntity {
                            id: actual_target,
                            value: target.solution(),
                        },
                    )?;
                    /* */// println!("after {:?}: {:?}, {:?}\n",  rule, origin, target);
                }
            }
        }

        Ok(())
    }

    /// finds a solution for the current set of constraints,
    /// returning an error in case there are either multiple or none.
    pub fn solve(&mut self) -> Result<TVec<EntityId, T::Result>, SolveError<T, E>> {
        #[cfg(feature = "profiler")]
        profile_scope!("solve");
        /* */// println!("{:?},\n{:?}", self.entities, self.productions);
        let mut all_ids: Box<[EntityId]> = (0..self.entities.len())
            .map(|v| EntityId(v))
            .collect::<Box<_>>();
        let mut step_count = 0;
        let mut first = true;
        while first || all_ids.iter().any(|&id| !self.entities[id].solved()) {
            first = false;
            let mut ids: &mut [EntityId] = &mut all_ids;
            while let Some(pos) = ids.iter().position(|&id| self.entities[id].solved()) {
                ids.swap(0, pos);
                self.apply_entity(ids[0])
                    .map_err(|e| SolveError::ProductionError(e))?;

                ids = &mut ids[1..];

                step_count += 1;
                if step_count > 1000 {
                    return Err(SolveError::UnsolvedEntities(
                        all_ids
                            .iter()
                            .filter(|&&id| !self.entities[id].solved())
                            .map(|&id| (id, self.entities[id].clone()))
                            .collect(),
                    ));
                }
            }
        }

        Ok(self.entities.iter_mut().map(|e| e.solution()).collect())
    }
}
