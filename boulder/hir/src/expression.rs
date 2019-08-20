use crate::*;

#[derive(Debug, Clone)]
pub enum Expression<'a, V> {
    Block(Meta<'a, ()>, Vec<Expression<'a, V>>),
    Variable(V),
    Lit(Meta<'a, Literal>),
    Binop(
        Meta<'a, Binop>,
        Box<Expression<'a, V>>,
        Box<Expression<'a, V>>,
    ),
    Statement(Meta<'a, ()>, Box<Expression<'a, V>>),
    Assignment(V, Box<Expression<'a, V>>),
}

impl<'a> Expression<'a, UnresolvedVariable<'a>> {
    pub fn resolve_variables(
        self,
        variables: &mut std::vec::Vec<function::Variable<'a, UnresolvedType>>,
        lookup: &mut Vec<Vec<(Box<str>, VariableId)>>,
    ) -> Result<Expression<'a, Meta<'a, VariableId>>, CompileError> {
        fn get_id<'b>(
            name: Meta<'b, Box<str>>,
            lookup: &mut Vec<Vec<(Box<str>, VariableId)>>,
        ) -> Result<Meta<'b, VariableId>, CompileError> {
            for scope in lookup.iter().rev() {
                for (var, id) in scope.iter().rev() {
                    if var == &name.item {
                        return Ok(name.replace(id.clone()));
                    }
                }
            }

            CompileError::new(
                &name,
                format_args!("Cannot find value `{}` in this scope", name.item),
            )
        }

        Ok(match self {
            Expression::Block(meta, expressions) => {
                lookup.push(Vec::new());
                let mut new = Vec::new();
                for expr in expressions {
                    new.push(expr.resolve_variables(variables, lookup)?);
                }
                lookup.pop();
                Expression::Block(meta, new)
            }
            Expression::Variable(var) => match var {
                UnresolvedVariable::Simple(name) => Expression::Variable(get_id(name, lookup)?),
                UnresolvedVariable::Typed(name, type_name) => {
                    let id = VariableId(variables.len());
                    let meta = name.simplify();
                    lookup.last_mut().unwrap().push((name.item.clone(), id));
                    variables.push(function::Variable {
                        name,
                        ty: type_name.map(|name| UnresolvedType::Named(name)),
                    });
                    Expression::Variable(meta.replace(id))
                }
            },
            Expression::Lit(lit) => Expression::Lit(lit),
            Expression::Binop(op, rhs, lhs) => Expression::Binop(
                op,
                Box::new(rhs.resolve_variables(variables, lookup)?),
                Box::new(lhs.resolve_variables(variables, lookup)?),
            ),
            Expression::Assignment(var, expr) => {
                let id = match var {
                    UnresolvedVariable::Simple(name) => get_id(name, lookup)?,
                    UnresolvedVariable::Typed(name, type_name) => {
                        let id = VariableId(variables.len());
                        let meta = name.simplify();
                        lookup.last_mut().unwrap().push((name.item.clone(), id));
                        variables.push(function::Variable {
                            name,
                            ty: type_name.map(|name| UnresolvedType::Named(name)),
                        });
                        meta.replace(id)
                    }
                };

                Expression::Assignment(id, Box::new(expr.resolve_variables(variables, lookup)?))
            }
            Expression::Statement(meta, expr) => {
                Expression::Statement(meta, Box::new(expr.resolve_variables(variables, lookup)?))
            }
        })
    }
}

impl<'a> Expression<'a, Meta<'a, VariableId>> {
    pub fn type_constraints(&self, constraints: &mut ty::Constraints<'a>) -> ty::EntityId {
        match self {
            Expression::Block(meta, v) => {
                if let Some((last, start)) = v.split_last() {
                    for e in start {
                        let _stmt = e.type_constraints(constraints);
                    }

                    last.type_constraints(constraints)
                } else {
                    constraints.add_entity(meta.clone(), ty::State::Solved(ty::EMPTY_ID))
                }
            }
            Expression::Variable(var) => constraints.convert_variable_id(var.clone()),
            Expression::Lit(lit) => match &lit.item {
                Literal::Integer(_) => {
                    let id = constraints.add_entity(lit.simplify(), ty::State::Open);
                    constraints.add_membership(id, ty::INTEGER_GROUP_ID);
                    id
                }
            },
            Expression::Binop(op, a, b) => match op.item {
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                    let a = a.type_constraints(constraints);
                    constraints.add_membership(a, ty::INTEGER_GROUP_ID);
                    let b = b.type_constraints(constraints);
                    constraints.add_membership(b, ty::INTEGER_GROUP_ID);
                    constraints.add_equality(a, b);
                    a
                }
            },
            Expression::Statement(_meta, expr) => {
                let _expr = expr.type_constraints(constraints);
                constraints.add_entity(self.span(), ty::State::Solved(ty::EMPTY_ID))
            }
            Expression::Assignment(var, expr) => {
                let expr = expr.type_constraints(constraints);
                let var_id = constraints.convert_variable_id(var.clone());
                constraints.add_equality(expr, var_id);
                constraints.add_entity(self.span(), ty::State::Solved(ty::EMPTY_ID))
            }
        }
    }

    pub fn to_mir(
        self,
        types: &[mir::Type],
        variables: &[TypeId],
        var_lookup: &mut [Option<mir::StepId>],
        curr: &mut mir::Block,
        func: &mut mir::Function,
    ) -> mir::StepId {
        match self {
            Expression::Block(meta, v) => {
                /*if let Some((last, start)) = v.split_last() {
                    for e in start {
                        let _stmt = e.type_constraints(constraints);
                    }
                } else {
                    constraints.add_entity(meta.clone(), ty::State::Solved(ty::EMPTY_ID))
                }*/
                mir::StepId(0)
            }
            /*Expression::Variable(var) => constraints.convert_variable_id(var.clone()),
            Expression::Lit(lit) => match &lit.item {
                Literal::Integer(_) => {
                    let id = constraints.add_entity(lit.simplify(), ty::State::Open);
                    constraints.add_membership(id, ty::INTEGER_GROUP_ID);
                    id
                }
            },
            Expression::Binop(op, a, b) => match op.item {
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                    let a = a.type_constraints(constraints);
                    constraints.add_membership(a, ty::INTEGER_GROUP_ID);
                    let b = b.type_constraints(constraints);
                    constraints.add_membership(b, ty::INTEGER_GROUP_ID);
                    constraints.add_equality(a, b);
                    a
                }
            },
            Expression::Statement(_meta, expr) => {
                let _expr = expr.type_constraints(constraints);
                constraints.add_entity(self.span(), ty::State::Solved(ty::EMPTY_ID))
            }
            Expression::Assignment(var, expr) => {
                let expr = expr.type_constraints(constraints);
                let var_id = constraints.convert_variable_id(var.clone());
                constraints.add_equality(expr, var_id);
                constraints.add_entity(self.span(), ty::State::Solved(ty::EMPTY_ID))
            }*/
            _ => unimplemented!(),
        }
    }
}

impl<'a, T: diagnostics::Span<'a>> diagnostics::Span<'a> for Expression<'a, T> {
    fn span(&self) -> Meta<'a, ()> {
        match self {
            Expression::Block(meta, _v) => meta.simplify(),
            Expression::Variable(var) => var.span(),
            Expression::Lit(lit) => lit.simplify(),
            Expression::Binop(_op, a, b) => a.span().append(b.span()),
            Expression::Statement(meta, expr) => expr.span().append(meta.clone()),
            Expression::Assignment(var, expr) => var.span().simplify().append(expr.span()),
        }
    }
}
