use std::collections::HashMap;

use anyhow::{anyhow, bail};

use crate::ast::{AtomTy, Expr, Ident, LitExpr, Pat, Ty};

pub fn eval(expr: &Expr, scope: &Scope) -> anyhow::Result<Expr> {
    println!("evaluating expression: {expr}");

    // std::thread::sleep(std::time::Duration::from_secs_f32(0.5));

    match expr {
        Expr::Call(expr, arg) => {
            let caller = eval(expr, scope)?;
            println!("caller: {expr} eval'd caller: {caller} callee: {arg}");
            match caller {
                Expr::Var(ident) => {
                    let expr = Expr::Call(Box::new(scope.get(&ident)?.clone()), arg.clone());
                    eval(&expr, scope)
                }
                Expr::Lit(lit_expr) => bail!(
                    "attempted to call a literal value of type `{}` \
                    (value `{lit_expr}`) with a value `{arg}`",
                    lit_expr.ty()
                ),
                Expr::Lambda(lambda) => {
                    println!("lambda: {lambda}");

                    match &**arg {
                        Expr::Call(arg, tail) => {
                            let arg = eval(arg, scope)?;

                            type_check(&lambda.arg.ty, &arg)?;

                            let expr = Expr::Call(lambda.expr, tail.clone());
                            let scope = scope.with_var(lambda.arg.name.clone(), arg.clone());
                            Ok(eval(&expr, &scope)?)
                        }
                        _ => {
                            let arg = eval(arg, scope)?;

                            type_check(&lambda.arg.ty, &arg)?;

                            // dbg!(&scope);

                            let scope = scope.with_var(lambda.arg.name.clone(), arg.clone());
                            eval(&lambda.expr, &scope)
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
                println!("checking arm: {arm}");

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
                    .ok_or_else(|| anyhow!("variable `{i}` not found in this scope"))
                    .and_then(|scope| scope.get(i))
            })
            .or_else(|x| x)
    }
}
