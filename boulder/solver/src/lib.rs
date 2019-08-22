use std::{collections::HashMap, hash::Hash, mem, ops::Index};

#[derive(Debug, Clone, Copy)]
pub struct EntityId(usize);

#[derive(Debug, Clone, Copy)]
pub struct ProductionId(usize);

type Production<T> = HashMap<T, Vec<T>>;

#[derive(Debug, Clone)]
pub enum Rule {
    Production(ProductionId, EntityId),
}

#[derive(Debug)]
pub struct ConstraintSolver<T: Eq + Hash> {
    all_entities: Vec<usize>,
    entities: Vec<Vec<T>>,
    rules: Vec<Vec<Rule>>,
    productions: Vec<Production<T>>,
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
pub enum SolveError<T> {
    EmptyValue(EntityId),
    DuplicateValues(EntityId, T, T),
}

impl<T: Eq + Hash + Clone + std::fmt::Debug> ConstraintSolver<T> {
    pub fn new() -> Self {
        Self {
            all_entities: Vec::new(),
            entities: Vec::new(),
            rules: Vec::new(),
            productions: Vec::new(),
        }
    }

    pub fn add_entity(&mut self, values: Vec<T>) -> EntityId {
        let id = EntityId(self.all_entities.len());
        let actual = self.entities.len();
        self.all_entities.push(actual);
        self.entities.push(values);
        self.rules.push(Vec::new());
        id
    }

    pub fn add_equality(&mut self, a: EntityId, b: EntityId) {
        let a = self.all_entities[a.0];
        let b = self.all_entities[b.0];
        if a != b {
            let upper = a.max(b);
            let lower = a.min(b);
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
    }

    pub fn add_production(&mut self, production: Production<T>) -> ProductionId {
        let id = ProductionId(self.productions.len());
        self.productions.push(production);
        id
    }

    fn arrange_ids(&self, ids: &mut [usize]) {
        if let Some((i, _)) = ids
            .iter().copied().enumerate()
            .min_by_key(|&(_, id)| self.entities[id].len())
        {
            ids.swap(0, i);
        }
    }

    fn solve_partial(&mut self, ids: &mut [usize]) -> Result<Vec<T>, SolveError<T>> {
        let id = ids[0];
        debug_assert_eq!(self.entities[id].len(), 1);
        let value = &self.entities[id].last().cloned().unwrap();
        for rule in self.rules[id].iter() {
            match rule {
                &Rule::Production(prod, target_id) => {
                    if let Some(values) = self.productions[prod.0].get(&value) {
                        let target = &mut self.entities[target_id.0];
                        target.retain(|v| values.contains(v));
                        if target.is_empty() {
                            return Err(SolveError::EmptyValue(target_id));
                        }
                    } else {
                        return Err(SolveError::EmptyValue(EntityId(id)));
                    }
                }
            }
        }

        if ids.len() > 1 {
            let ids = &mut ids[1..];
            self.arrange_ids(ids);
            let id = ids[0];
            let mut result = None;
            for v in mem::replace(&mut self.entities[id], Vec::new()) {
                let clone = self.entities.clone();
                self.entities[id].push(v);
                match self.solve_partial(ids) {
                    Ok(mut res) => match result {
                        None => result = Some(res),
                        Some(mut old) => {
                            debug_assert_eq!(id, ids[0]);
                            return Err(SolveError::DuplicateValues(
                                EntityId(ids[0]),
                                old.swap_remove(id),
                                res.swap_remove(id),
                            ));
                        }
                    },
                    Err(SolveError::EmptyValue(_)) => (),
                    err @ Err(SolveError::DuplicateValues(_, _, _)) => return err,
                }
                self.entities = clone;
            }

            match result {
                None => Err(SolveError::EmptyValue(EntityId(ids[0]))),
                Some(res) => Ok(res),
            }
        } else {
            Ok(mem::replace(&mut self.entities, Vec::new())
                .into_iter()
                .map(|mut e| {
                    debug_assert_eq!(e.len(), 1);
                    e.pop().unwrap()
                })
                .collect())
        }
    }

    /// finds a solution for the current set of constraints,
    /// returning an error in case there are either multiple or none.
    pub fn solve(mut self) -> Result<Solution<T>, SolveError<T>> {
        dbg!(&self);
        if let Some((i, _)) = self.entities.iter().enumerate().find(|(_, e)| e.is_empty()) {
            return Err(SolveError::EmptyValue(EntityId(i)));
        }

        let mut ids: Vec<usize> = (0..self.entities.len()).map(|v| v).collect();
        self.arrange_ids(&mut ids);
        let id = ids[0];
        let mut result = None;
        for v in mem::replace(&mut self.entities[id], Vec::new()) {
            let clone = self.entities.clone();
            self.entities[id].push(v);
            match self.solve_partial(&mut ids) {
                Ok(mut res) => match result {
                    None => result = Some(res),
                    Some(mut old) => {
                        debug_assert_eq!(id, ids[0]);
                        return Err(SolveError::DuplicateValues(
                            EntityId(ids[0]),
                            old.swap_remove(id),
                            res.swap_remove(id),
                        ));
                    }
                },
                Err(SolveError::EmptyValue(_)) => (),
                Err(err @ SolveError::DuplicateValues(_, _, _)) => return Err(err),
            }
            self.entities = clone;
        }

        match result {
            None => Err(SolveError::EmptyValue(EntityId(ids[0]))),
            Some(res) => Ok(Solution {
                inner: self
                    .all_entities
                    .into_iter()
                    .map(|e| res[e].clone())
                    .collect(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn equality() {
        let mut solver = ConstraintSolver::new();
        let a = solver.add_entity(vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
        let b = solver.add_entity(vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
        let c = solver.add_entity(vec![0, 1, 2, 3, 4]);
        let d = solver.add_entity(vec![3]);
        solver.add_equality(a, b);
        solver.add_equality(b, c);
        solver.add_equality(d, b);
        let solution = solver.solve().unwrap();
        assert_eq!(solution[a], 3);
        assert_eq!(solution[b], 3);
        assert_eq!(solution[c], 3);
        assert_eq!(solution[d], 3);
    }

    #[test]
    fn production() {
        let mut solver = ConstraintSolver::new();
        let a = solver.add_entity(vec![0]);
        let b = solver.add_entity(vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
        let c = solver.add_entity(vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
        let d = solver.add_entity(vec![7]);
        
        solver.add_production({
            let mut production = HashMap::new();
            production.insert(0, vec![7]);
            production.insert(3, vec![7]);
            production
        });
        solver.add_equality(b, c);
        solver.add_equality(b, d);
        let solution = solver.solve().unwrap();
        assert_eq!(solution[a], 0);
        assert_eq!(solution[b], 7);
        assert_eq!(solution[c], 7);
        assert_eq!(solution[d], 7);
    }
}
