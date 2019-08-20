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
            Expression::Variable(var) =>
                match var {
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
                }
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
            _ => unimplemented!(),
        })
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
