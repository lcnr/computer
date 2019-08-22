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

    pub fn add_rule(&mut self, entity: EntityId, rule: Rule) {
        self.rules[entity.0].push(rule);
    }

    pub fn extend_production(&mut self, production: ProductionId, orig: T, mut res: Vec<T>) {
        self.productions[production.0].entry(orig).or_default().append(&mut res);
    }

    fn get_id<'a>(&self, ids: &'a mut [usize]) -> (usize, &'a mut [usize]) {
        if let Some((i, _)) = ids
            .iter().copied().enumerate()
            .min_by_key(|&(_, id)| self.entities[id].len())
        {
            ids.swap(0, i);
        }

        (ids[0], &mut ids[1..])
    }

    fn last_equal_entity(&self, id: usize) -> EntityId {
        EntityId(self.all_entities.iter().enumerate().rev().find(|&(_, &entity)| id == entity).unwrap().0)
    }

    fn solve_partial(&mut self, ids: &mut [usize]) -> Result<Vec<T>, SolveError<T>> {
        let (id , ids) = self.get_id(ids);
        let mut old_result: Option<Vec<T>> = None;
        for value in mem::replace(&mut self.entities[id], Vec::new()) {
            let clone = self.entities.clone();
            self.entities[id].push(value);
            for rule in self.rules[id].iter() {
                match rule {
                    &Rule::Production(prod, target_id) => {
                        if let Some(values) = self.productions[prod.0].get(&self.entities[id][0]) {
                            let target = &mut self.entities[self.all_entities[target_id.0]];
                            target.retain(|v| values.contains(v));
                            if target.is_empty() {
                                self.entities = clone;
                                return Err(SolveError::EmptyValue(target_id));
                            }
                        } else {
                            self.entities = clone;
                            return Err(SolveError::EmptyValue(EntityId(id)));
                        }
                    }
                }
            }

            if ids.is_empty() {
                old_result = Some(mem::replace(&mut self.entities, clone).into_iter().map(|mut e| {
                    let t = e.pop().unwrap();
                    debug_assert_eq!(e.len(), 0);
                    t
                }).collect())
            } else {
                let result = self.solve_partial(ids);
                self.entities = clone;
                match result {
                    Ok(result) => if let Some(old) = old_result {
                        return Err(SolveError::DuplicateValues(self.last_equal_entity(id), old[id].clone(), result[id].clone()));
                    } else {
                        old_result = Some(result);
                    }
                    Err(SolveError::EmptyValue(_)) => (),
                    err @Err(SolveError::DuplicateValues(_, _, _)) => return err,
                }
            }
        }

        if let Some(res) = old_result {
            Ok(res)
        } else {
            Err(SolveError::EmptyValue(self.last_equal_entity(id)))
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
        let result = self.solve_partial(&mut ids)?;

        Ok(Solution {
            inner: self
                    .all_entities
                    .into_iter()
                    .map(|e| result[e].clone())
                    .collect(),
        })
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
