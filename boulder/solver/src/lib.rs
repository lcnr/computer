use std::{collections::HashMap, hash::Hash, mem, ops::Index};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EntityId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProductionId(usize);

type Production<T> = HashMap<T, T>;

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
pub struct ExpectedFound<T> {
    pub expected: (EntityId, Vec<T>),
    pub found: (EntityId, Vec<T>),
}

#[derive(Debug)]
pub enum SolveError<T> {
    UnsolvedEntities(Vec<(EntityId, Vec<T>)>),
    ApplyRuleError(ApplyRuleError<T>),
}

#[derive(Debug)]
pub enum ApplyRuleError<T> {
    /// production, object, object_type, field_id
    InvalidProduction(ProductionId, EntityId, T, EntityId),
    /// production, field_id, production_type, possible_field_types
    InvalidProductionResult(ProductionId, EntityId, T, Vec<T>),
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

    pub fn add_production(&mut self, production: Production<T>) -> ProductionId {
        let id = ProductionId(self.productions.len());
        self.productions.push(production);
        id
    }

    pub fn add_rule(&mut self, entity: EntityId, rule: Rule) {
        self.rules[entity.0].push(rule);
    }

    pub fn extend_production(&mut self, production: ProductionId, orig: T, res: T) {
        assert!(
            self.productions[production.0].insert(orig, res).is_none(),
            "Tried to overwrite an existing production rule"
        );
    }

    fn apply_entity(&mut self, id: usize) -> Result<(), ApplyRuleError<T>> {
        for rule in self.rules[id].iter() {
            match rule {
                &Rule::Production(prod, target_id) => {
                    if let Some(value) = self.productions[prod.0].get(&self.entities[id][0]) {
                        let target = &mut self.entities[self.all_entities[target_id.0]];
                        if target.contains(value) {
                            target.clear();
                            target.push(value.clone());
                        } else {
                            return Err(ApplyRuleError::InvalidProductionResult(
                                prod,
                                target_id,
                                value.clone(),
                                mem::replace(target, Vec::new()),
                            ));
                        }
                    } else {
                        return Err(ApplyRuleError::InvalidProduction(
                            prod,
                            EntityId(id),
                            self.entities[id].pop().unwrap(),
                            target_id,
                        ));
                    }
                }
            }
        }

        Ok(())
    }

    /// finds a solution for the current set of constraints,
    /// returning an error in case there are either multiple or none.
    pub fn solve(mut self) -> Result<Solution<T>, SolveError<T>> {
        if let Some((i, _)) = self.entities.iter().enumerate().find(|(_, e)| e.is_empty()) {
            panic!("Entity without possible state: {}", i);
        }

        let mut ids: &mut [usize] = &mut (0..self.entities.len()).map(|v| v).collect::<Box<_>>();
        while let Some(pos) = ids.iter().position(|&id| self.entities[id].len() == 1) {
            ids.swap(0, pos);
            self.apply_entity(ids[0])
                .map_err(|e| SolveError::ApplyRuleError(e))?;
            ids = &mut ids[1..];
        }

        if ids.is_empty() {
            let entities = self.entities;
            Ok(Solution {
                inner: self
                    .all_entities
                    .into_iter()
                    .map(|e| entities[e].last().unwrap().clone())
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
        solver.add_equality(a, b).unwrap();
        solver.add_equality(b, c).unwrap();
        solver.add_equality(d, b).unwrap();
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
            production.insert(0, 7);
            production.insert(3, 7);
            production
        });
        solver.add_equality(b, c).unwrap();
        solver.add_equality(b, d).unwrap();
        let solution = solver.solve().unwrap();
        assert_eq!(solution[a], 0);
        assert_eq!(solution[b], 7);
        assert_eq!(solution[c], 7);
        assert_eq!(solution[d], 7);
    }
}
