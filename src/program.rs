use std::collections::HashMap;

use anyhow::{anyhow, bail};
use tracing::debug;

use crate::ast::{AtomTy, Block, CaseArm, Expr, Ident, Lambda, LitExpr, Pat, Stmt, Ty};

pub fn eval(expr: &Expr, scope: &Scope) -> anyhow::Result<Expr> {
    debug!("evaluating expression: {expr}");

    // std::thread::sleep(std::time::Duration::from_secs_f32(0.5));

    // dbg!(scope);

    match expr {
        Expr::Call(expr, arg) => {
            let caller = eval(expr, scope)?;
            debug!("caller: {expr} eval'd caller: {caller} callee: {arg}");

            match caller {
                Expr::Var(ident) => {
                    let expr = Expr::Call(Box::new(scope.get(&ident)?.clone()), arg.clone());
                    eval(&expr, scope)
                }
                Expr::Lit(lit_expr) => bail!(
                    "attempted to call a literal value of type `{}` \
                    (value `{lit_expr}`) with value `{arg}`",
                    lit_expr.ty()
                ),
                Expr::Lambda(lambda) => {
                    debug!("lambda: {lambda}");

                    match &**arg {
                        Expr::Call(arg, tail) => {
                            let arg = eval(arg, scope)?;

                            type_check(&lambda.arg.ty, &arg)?;

                            let expr = substitute(&lambda.expr, &lambda.arg.name, &arg);

                            let expr = Expr::Call(Box::new(expr), tail.clone());

                            let scope = scope.with_var(lambda.arg.name.clone(), arg.clone());

                            eval(&expr, &scope)
                        }
                        _ => {
                            let arg = eval(arg, scope)?;

                            type_check(&lambda.arg.ty, &arg)?;

                            // dbg!(&scope);

                            let expr = substitute(&lambda.expr, &lambda.arg.name, &arg);

                            let scope = scope.with_var(lambda.arg.name.clone(), arg.clone());

                            eval(&expr, &scope)
                        }
                    }
                }
                Expr::Call(caller, caller2) => {
                    let expr = Expr::Call(caller, Box::new(Expr::Call(caller2, arg.clone())));

                    eval(&expr, scope)
                }
                Expr::Block(_block) => todo!(),
                Expr::Case(_expr, _vec) => todo!(),
                Expr::Builtin(builtin) => eval(&Expr::Builtin(builtin), scope),
                Expr::Tuple(_tuple) => bail!("attempted to call a tuple"),
            }
        }
        Expr::Block(block) => {
            let scope = if block.stmts.is_empty() {
                scope
            } else {
                &Scope {
                    parent: Some(scope),
                    vars: block
                        .stmts
                        .clone()
                        .into_iter()
                        .map(|s| (s.ident, s.value))
                        .collect(),
                }
            };

            eval(&block.tail, scope)
        }
        Expr::Case(expr, arms) => {
            let expr = eval(expr, scope)?;

            for arm in arms {
                debug!("checking arm: {arm}");

                match (&arm.pat, &expr) {
                    (Pat::Var(ident), _) => {
                        return eval(&arm.expr, &scope.with_var(ident.clone(), expr.clone()));
                    }
                    (Pat::Num(_), Expr::Var(_ident)) => todo!(),
                    (Pat::Num(pat), Expr::Lit(LitExpr::Int(n))) if pat == n => {
                        return eval(&arm.expr, scope);
                    }
                    (Pat::Num(_), Expr::Lambda(_lambda)) => todo!(),
                    (Pat::Num(_), Expr::Call(_expr, _expr1)) => todo!(),
                    (Pat::Num(_), Expr::Block(_block)) => todo!(),
                    (Pat::Num(_), Expr::Case(_expr, _vec)) => todo!(),
                    (Pat::Num(_), Expr::Tuple(_vec)) => todo!(),
                    (Pat::Num(_), Expr::Builtin(_builtin)) => todo!(),
                    (Pat::Wildcard, _) => {
                        return eval(&arm.expr, scope);
                    }
                    _ => {}
                }
            }

            bail!("no arms matched")
        }
        Expr::Var(var) => eval(scope.get(var)?, scope),
        Expr::Tuple(exprs) => {
            if exprs.len() == 1 {
                eval(&exprs[0], scope)
            } else {
                Ok(Expr::Tuple(
                    exprs
                        .iter()
                        .map(|e| eval(e, scope))
                        .collect::<Result<_, _>>()?,
                ))
            }
        }
        Expr::Builtin(builtin) => builtin.call(scope),
        e => Ok(e.clone()),
    }
}

pub fn substitute(expr: &Expr, var: &Ident, value: &Expr) -> Expr {
    match expr {
        Expr::Var(ident) => {
            if ident == var {
                value.clone()
            } else {
                Expr::Var(ident.clone())
            }
        }
        Expr::Lambda(lambda) => {
            if lambda.arg.name != *var {
                Expr::Lambda(Lambda {
                    arg: lambda.arg.clone(),
                    ret: lambda.ret.clone(),
                    expr: Box::new(substitute(&lambda.expr, var, value)),
                })
            } else {
                Expr::Lambda(lambda.clone())
            }
        }
        Expr::Call(caller, arg) => Expr::Call(
            Box::new(substitute(caller, var, value)),
            Box::new(substitute(arg, var, value)),
        ),
        Expr::Block(block) => {
            if block.stmts.iter().any(|s| s.ident == *var) {
                Expr::Block(block.clone())
            } else {
                Expr::Block(Block {
                    stmts: block
                        .stmts
                        .iter()
                        .map(|s| Stmt {
                            ident: s.ident.clone(),
                            value: substitute(&s.value, var, value),
                        })
                        .collect(),
                    tail: Box::new(substitute(&block.tail, var, value)),
                })
            }
        }
        Expr::Case(expr, arms) => Expr::Case(
            Box::new(substitute(expr, var, value)),
            arms.iter()
                .map(|arm| CaseArm {
                    pat: arm.pat.clone(),
                    expr: Box::new(substitute(&arm.expr, var, value)),
                })
                .collect(),
        ),
        Expr::Tuple(tuple) => {
            Expr::Tuple(tuple.iter().map(|t| substitute(t, var, value)).collect())
        }
        _ => expr.clone(),
    }
}

fn type_check(head_arg_ty: &Ty, arg: &Expr) -> anyhow::Result<()> {
    match (&head_arg_ty, arg) {
        (Ty::Atom(AtomTy::Int), Expr::Lit(LitExpr::Int(_))) => Ok(()),
        (Ty::Atom(AtomTy::Str), Expr::Lit(LitExpr::Str(_))) => Ok(()),
        (Ty::Atom(AtomTy::Bool), Expr::Lit(LitExpr::Bool(_))) => Ok(()),
        (ty, Expr::Block(block)) => type_check(ty, &block.tail),
        (Ty::Tuple(ty), Expr::Tuple(e)) => {
            ty.iter().zip(e).try_for_each(|(ty, e)| type_check(ty, e))
        }
        (ty, Expr::Call(head, _)) => type_check(ty, head),
        (f @ Ty::Fn(_, _), Expr::Lambda(lambda)) => {
            if lambda.sig() == **f {
                Ok(())
            } else {
                Err(anyhow!(
                    "attempted to use a lambda of type `{lambda}` as a lambda of type `{f}`"
                ))
            }
        }
        _ => Err(anyhow!(
            "attempted to use a value of type `{arg}` as a value of type `{head_arg_ty}`"
        )),
    }
}

pub struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    vars: HashMap<Ident, Expr>,
}

impl std::fmt::Debug for Scope<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("Scope");

        if let Some(parent) = self.parent {
            d.field("parent", parent);
        }

        d.field("vars", &self.vars).finish()
    }
}

impl<'s> Scope<'s> {
    pub fn new(vars: HashMap<Ident, Expr>) -> Self {
        Self { parent: None, vars }
    }

    pub fn with_var<'a>(&'a self, var: Ident, expr: Expr) -> Scope<'a>
    where
        'a: 's,
    {
        Self {
            parent: Some(self),
            vars: [(var, expr)].into_iter().collect(),
        }
    }

    pub fn get(&self, i: &Ident) -> anyhow::Result<&Expr> {
        self.vars
            .get(i)
            .ok_or_else(|| {
                self.parent
                    .as_ref()
                    .ok_or_else(|| anyhow!("`{i}` not found in this scope"))
                    .and_then(|scope| scope.get(i))
            })
            .or_else(|x| x)
    }
}
