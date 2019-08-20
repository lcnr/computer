use crate::*;

use crate::ty::EntityId;

#[derive(Debug, Clone)]
pub enum Expression<'a, V, N> {
    Block(N, Meta<'a, ()>, Vec<Expression<'a, V, N>>),
    Variable(N, V),
    Lit(N, Meta<'a, Literal>),
    Binop(
        N,
        Meta<'a, Binop>,
        Box<Expression<'a, V, N>>,
        Box<Expression<'a, V, N>>,
    ),
    Statement(N, Meta<'a, ()>, Box<Expression<'a, V, N>>),
    Assignment(N, V, Box<Expression<'a, V, N>>),
}

impl<'a> Expression<'a, UnresolvedVariable<'a>, ()> {
    pub fn resolve_variables(
        self,
        variables: &mut std::vec::Vec<function::Variable<'a, UnresolvedType>>,
        lookup: &mut Vec<Vec<(Box<str>, VariableId)>>,
    ) -> Result<Expression<'a, Meta<'a, VariableId>, ()>, CompileError> {
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
            Expression::Block((), meta, expressions) => {
                lookup.push(Vec::new());
                let mut new = Vec::new();
                for expr in expressions {
                    new.push(expr.resolve_variables(variables, lookup)?);
                }
                lookup.pop();
                Expression::Block((), meta, new)
            }
            Expression::Variable((), var) => match var {
                UnresolvedVariable::Simple(name) => Expression::Variable((), get_id(name, lookup)?),
                UnresolvedVariable::Typed(name, type_name) => {
                    let id = VariableId(variables.len());
                    let meta = name.simplify();
                    lookup.last_mut().unwrap().push((name.item.clone(), id));
                    variables.push(function::Variable {
                        name,
                        ty: type_name.map(|name| UnresolvedType::Named(name)),
                    });
                    Expression::Variable((), meta.replace(id))
                }
            },
            Expression::Lit((), lit) => Expression::Lit((), lit),
            Expression::Binop((), op, rhs, lhs) => Expression::Binop(
                (),
                op,
                Box::new(rhs.resolve_variables(variables, lookup)?),
                Box::new(lhs.resolve_variables(variables, lookup)?),
            ),
            Expression::Assignment((), var, expr) => {
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

                Expression::Assignment((), id, Box::new(expr.resolve_variables(variables, lookup)?))
            }
            Expression::Statement((), meta, expr) => Expression::Statement(
                (),
                meta,
                Box::new(expr.resolve_variables(variables, lookup)?),
            ),
        })
    }
}

impl<'a> Expression<'a, Meta<'a, VariableId>, ()> {
    pub fn type_constraints(
        self,
        constraints: &mut ty::Constraints<'a>,
    ) -> Expression<'a, Meta<'a, VariableId>, EntityId> {
        match self {
            Expression::Block((), meta, v) => {
                let (id, content) = if v.is_empty() {
                    (
                        constraints.add_entity(meta.simplify(), ty::State::Solved(ty::EMPTY_ID)),
                        Vec::new(),
                    )
                } else {
                    let content = v
                        .into_iter()
                        .map(|expr| expr.type_constraints(constraints))
                        .collect::<Vec<_>>();
                    let id = content.last().unwrap().id();
                    (id, content)
                };
                Expression::Block(id, meta, content)
            }
            Expression::Variable((), var) => {
                Expression::Variable(constraints.convert_variable_id(var.clone()), var)
            }
            Expression::Lit((), lit) => match &lit.item {
                Literal::Integer(_) => {
                    let id = constraints.add_entity(lit.simplify(), ty::State::Open);
                    constraints.add_membership(id, ty::INTEGER_GROUP_ID);
                    Expression::Lit(id, lit)
                }
            },
            Expression::Binop((), op, a, b) => match op.item {
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                    let a = a.type_constraints(constraints);
                    constraints.add_membership(a.id(), ty::INTEGER_GROUP_ID);
                    let b = b.type_constraints(constraints);
                    constraints.add_membership(b.id(), ty::INTEGER_GROUP_ID);
                    constraints.add_equality(a.id(), b.id());
                    Expression::Binop(a.id(), op, Box::new(a), Box::new(b))
                }
            },
            Expression::Statement((), meta, expr) => {
                let span = expr.span().simplify().append(meta.clone());
                let expr = expr.type_constraints(constraints);
                Expression::Statement(
                    constraints.add_entity(span, ty::State::Solved(ty::EMPTY_ID)),
                    meta,
                    Box::new(expr),
                )
            }
            Expression::Assignment((), var, expr) => {
                let span = var.span().simplify().append(expr.span());
                let expr = expr.type_constraints(constraints);
                let var_id = constraints.convert_variable_id(var.clone());
                constraints.add_equality(expr.id(), var_id);
                Expression::Assignment(
                    constraints.add_entity(span, ty::State::Solved(ty::EMPTY_ID)),
                    var,
                    Box::new(expr),
                )
            }
        }
    }
}

impl<'a> Expression<'a, Meta<'a, VariableId>, EntityId> {
    pub fn id(&self) -> EntityId {
        match self {
            &Expression::Block(id, _, _)
            | &Expression::Variable(id, _)
            | &Expression::Lit(id, _)
            | &Expression::Binop(id, _, _, _)
            | &Expression::Statement(id, _, _)
            | &Expression::Assignment(id, _, _) => id,
        }
    }

    pub fn insert_types(self, types: &[TypeId]) -> Expression<'a, Meta<'a, VariableId>, TypeId> {
        match self {
            Expression::Block(id, meta, v) => {
                let ty = types[id.0];
                let content = v
                    .into_iter()
                    .map(|expr| expr.insert_types(types))
                    .collect::<Vec<_>>();
                Expression::Block(ty, meta, content)
            }
            Expression::Variable(id, var) => Expression::Variable(types[id.0], var),
            Expression::Lit(id, lit) => match &lit.item {
                Literal::Integer(_) => Expression::Lit(types[id.0], lit),
            },
            Expression::Binop(id, op, a, b) => match op.item {
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                    let a = a.insert_types(types);
                    let b = b.insert_types(types);
                    Expression::Binop(types[id.0], op, Box::new(a), Box::new(b))
                }
            },
            Expression::Statement(id, meta, expr) => {
                let expr = expr.insert_types(types);
                Expression::Statement(types[id.0], meta, Box::new(expr))
            }
            Expression::Assignment(id, var, expr) => {
                let expr = expr.insert_types(types);
                Expression::Assignment(types[id.0], var, Box::new(expr))
            }
        }
    }
}

impl<'a> Expression<'a, Meta<'a, VariableId>, TypeId> {
    pub fn to_mir(
        self,
        types: &[mir::Type],
        variables: &[TypeId],
        var_lookup: &mut [Option<mir::StepId>],
        curr: &mut mir::Block,
        func: &mut mir::Function,
    ) -> mir::StepId {
        match self {
            Expression::Block(ty, meta, v) => {
                /*if let Some((last, start)) = v.split_last() {
                    for e in start {
                        let _stmt = e.type_constraints(constraints);
                    }
                } else {
                    constraints.add_entity(meta.clone(), ty::State::Solved(ty::EMPTY_ID))
                }*/
                mir::StepId(0)
            }
            /*Expression::Variable((), var) => constraints.convert_variable_id(var.clone()),
            Expression::Lit((), lit) => match &lit.item {
                Literal::Integer(_) => {
                    let id = constraints.add_entity(lit.simplify(), ty::State::Open);
                    constraints.add_membership(id, ty::INTEGER_GROUP_ID);
                    id
                }
            },
            Expression::Binop((), op, a, b) => match op.item {
                Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
                    let a = a.type_constraints(constraints);
                    constraints.add_membership(a, ty::INTEGER_GROUP_ID);
                    let b = b.type_constraints(constraints);
                    constraints.add_membership(b, ty::INTEGER_GROUP_ID);
                    constraints.add_equality(a, b);
                    a
                }
            },
            Expression::Statement((), _meta, expr) => {
                let _expr = expr.type_constraints(constraints);
                constraints.add_entity(self.span(), ty::State::Solved(ty::EMPTY_ID))
            }
            Expression::Assignment((), var, expr) => {
                let expr = expr.type_constraints(constraints);
                let var_id = constraints.convert_variable_id(var.clone());
                constraints.add_equality(expr, var_id);
                constraints.add_entity(self.span(), ty::State::Solved(ty::EMPTY_ID))
            }*/
            _ => unimplemented!(),
        }
    }
}

impl<'a, T: diagnostics::Span<'a>, N> diagnostics::Span<'a> for Expression<'a, T, N> {
    fn span(&self) -> Meta<'a, ()> {
        match self {
            Expression::Block(_, meta, _) => meta.simplify(),
            Expression::Variable(_, var) => var.span(),
            Expression::Lit(_, lit) => lit.simplify(),
            Expression::Binop(_, _op, a, b) => a.span().append(b.span()),
            Expression::Statement(_, meta, expr) => expr.span().append(meta.clone()),
            Expression::Assignment(_, var, expr) => var.span().simplify().append(expr.span()),
        }
    }
}
