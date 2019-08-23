use std::{hash::Hash, mem, ops::Index, fmt};

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
pub struct ExpectedFound<T> {
    pub expected: (EntityId, Vec<T>),
    pub found: (EntityId, Vec<T>),
}

#[derive(Debug)]
pub enum SolveError<T, E> {
    UnsolvedEntities(Vec<(EntityId, Vec<T>)>),
    ProductionError(E),
}

pub struct Entity<'a, T> {
    pub id: EntityId,
    pub content: &'a mut Vec<T>
}

pub type Production<C, T, E> = Box<dyn FnMut(&mut C, Entity<T>, Entity<T>) -> Result<(), E>>;

pub struct ConstraintSolver<C, T, E> {
    all_entities: Vec<usize>,
    entities: Vec<Vec<T>>,
    rules: Vec<Vec<Rule>>,
    productions: Vec<Production<C, T, E>>,
    context: C,
}

impl<'ctx, C: fmt::Debug, T: fmt::Debug, E> fmt::Debug for ConstraintSolver<C, T, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ConstraintSolver")
        .field("all_entities", &self.all_entities)
        .field("entities", &self.entities)
        .field("rules", &self.rules)
        .field("context", &self.context)
        .field("productions", &"_")
        .finish()
    }
}

impl<C, T: Eq + Hash + Clone + std::fmt::Debug, E> ConstraintSolver<C, T, E> {
    pub fn new(context: C) -> Self {
        Self {
            all_entities: Vec::new(),
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
        let id = EntityId(self.all_entities.len());
        let actual = self.entities.len();
        self.all_entities.push(actual);
        self.entities.push(values);
        self.rules.push(Vec::new());
        id
    }

    pub fn add_equality(&mut self, a: EntityId, b: EntityId) -> Result<(), ExpectedFound<T>> {
        let a_id = self.all_entities[a.0];
        let b_id = self.all_entities[b.0];
        if !self.entities[a_id]
            .iter()
            .any(|v| self.entities[b_id].contains(v))
        {
            return Err(ExpectedFound {
                expected: (a, mem::replace(&mut self.entities[a_id], Vec::new())),
                found: (b, mem::replace(&mut self.entities[b_id], Vec::new())),
            })?;
        }

        if a_id != b_id {
            let upper = a_id.max(b_id);
            let lower = a_id.min(b_id);
            for entity in self.all_entities.iter_mut() {
                if *entity > upper {
                    *entity -= 1;
                } else if *entity == upper {
                    *entity = lower;
                }
            }

            let values = self.entities.remove(upper);
            let mut rules = self.rules.remove(upper);
            self.entities[lower].retain(|v| values.contains(v));
            self.rules[lower].append(&mut rules);
        }

        Ok(())
    }

    pub fn define_production(&mut self, production: Production<C, T, E>) -> ProductionId {
        let id = ProductionId(self.productions.len());
        self.productions.push(production);
        id
    }

    pub fn add_production(&mut self, production: ProductionId, origin: EntityId, target: EntityId) {
        assert_ne!(self.all_entities[origin.0], self.all_entities[target.0], "invalid production");
        self.rules[self.all_entities[origin.0]].push(Rule::ForwardProduction(production, origin, target));
        self.rules[self.all_entities[target.0]].push(Rule::BackwardsProduction(production, origin, target));
    }

    fn apply_entity(&mut self, id: usize) -> Result<(), E> {
        for rule in self.rules[id].iter() {
            match rule {
                &Rule::ForwardProduction(prod, actual_origin, actual_target) => {
                    let target_id = self.all_entities[actual_target.0];

                    let (begin, end) = dbg!(self.entities.split_at_mut(id.max(target_id)));
                    let (origin, target) = if target_id > id {
                        (&mut begin[id], &mut end[0])
                    } else {
                        (&mut end[0], &mut begin[target_id])
                    };
                    let production = &mut self.productions[prod.0];
                    production(&mut self.context, Entity {
                        id: actual_origin,
                        content: origin,
                    },
                    Entity {
                        id: actual_target,
                        content: target,
                    })?;
                }
                &Rule::BackwardsProduction(prod, actual_origin, actual_target) => {
                    let origin_id = self.all_entities[actual_origin.0];
                    let (begin, end) = self.entities.split_at_mut(id.max(origin_id));
                    let (origin, target) = if id > origin_id {
                        (&mut begin[origin_id], &mut end[0])
                    } else {
                        (&mut end[0], &mut begin[id])
                    };

                    let production = &mut self.productions[prod.0];
                    production(&mut self.context, Entity {
                        id: actual_origin,
                        content: origin,
                    },
                    Entity {
                        id: actual_target,
                        content: target,
                    })?;
                }
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

        let mut ids: &mut [usize] = &mut (0..self.entities.len()).map(|v| v).collect::<Box<_>>();
        while let Some(pos) = ids.iter().position(|&id| self.entities[id].len() == 1) {
            ids.swap(0, pos);
            self.apply_entity(ids[0])
                .map_err(|e| SolveError::ProductionError(e))?;
            ids = &mut ids[1..];
        }

        if ids.is_empty() {
            Ok(Solution {
                inner: self
                    .all_entities
                    .iter()
                    .map(|&e| self.entities[e].last().unwrap().clone())
                    .collect(),
            })
        } else {
            Err(SolveError::UnsolvedEntities(
                ids.iter()
                    .map(|&id| (EntityId(id), self.entities[id].clone()))
                    .collect(),
            ))
        }
    }
}
