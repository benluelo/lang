use std::collections::HashMap;

use anyhow::{anyhow, bail};

use crate::ast::{AtomTy, Expr, Ident, Lambda, LitExpr, Pat, Ty};

pub fn eval(expr: &Expr, scope: Option<&Scope>) -> anyhow::Result<Expr> {
    let scope = match scope {
        Some(scope) => scope,
        None => &Scope {
            parent: None,
            vars: HashMap::new(),
        },
    };

    println!("evaluating expression: {expr}");

    // std::thread::sleep(std::time::Duration::from_secs_f32(0.5));

    match expr {
        Expr::Call(expr, arg) => {
            let caller = eval(expr, Some(scope))?;
            println!("caller: {expr} eval'd caller: {caller} callee: {arg}");
            match caller {
                Expr::Var(ident) => {
                    let expr = Expr::Call(Box::new(scope.get(&ident)?.clone()), arg.clone());
                    eval(
                                    &expr,
                                    Some(scope),
                                )
                },
                Expr::Lit(lit_expr) => bail!(
                    "attempted to call a value of type `{}` (value `{lit_expr}`) with a value `{arg}`",
                    lit_expr.ty()
                ),
                // Expr::Lambda(lambda) => match lambda.arg.split_first() {
                //     Some((head_arg, tail_args)) => {
                //         assert!(type_check(&head_arg.ty, arg));

                //         let scope = Scope {
                //             parent: Some(scope),
                //             vars: [(head_arg.name.clone(), (**arg).clone())]
                //                 .into_iter()
                //                 .collect(),
                //         };

                //         eval(
                //             &Expr::Lambda(Lambda {
                //                 arg: tail_args.to_vec(),
                //                 expr: lambda.expr,
                //                 ret: lambda.ret,
                //             }),
                //             Some(scope),
                //         )
                //     }
                //     None => eval(&lambda.expr, Some(scope)),
                // },
                Expr::Lambda(lambda) => {
                    println!("lambda: {lambda}");

                    match &**arg {
                        Expr::Call(arg, tail) => {
                            let arg = eval(arg, Some(scope))?;

                            assert!(type_check(&lambda.arg.ty, &arg));

                            let expr = Expr::Call(lambda.expr, tail.clone());
                            let scope = scope.with_var(lambda.arg.name.clone(), arg.clone());
                            Ok(eval(&expr, Some(&scope))?)
                        }
                        _ => {
                            let arg = eval(arg, Some(scope))?;

                            assert!(type_check(&lambda.arg.ty, &arg));

                            // dbg!(&scope);

                            let scope = scope.with_var(lambda.arg.name.clone(), arg.clone());
                            eval(&lambda.expr, Some(&scope))
                        }
                    }
                }
                Expr::Call(caller, caller2) => {
                    let expr = Expr::Call(caller, Box::new(Expr::Call(caller2, arg.clone())));

                    eval(&expr, Some(scope))
                },
                Expr::Block(_block) => todo!(),
                Expr::Case(_expr, _vec) => todo!(),
                Expr::Builtin(builtin) => eval(&Expr::Builtin(builtin), Some(scope)),
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

            eval(&block.tail, Some(scope))
        }
        Expr::Case(expr, arms) => {
            let expr = eval(expr, Some(scope))?;

            for arm in arms {
                println!("checking arm: {arm}");

                match (&arm.pat, &expr) {
                    (Pat::Var(ident), _) => {
                        return eval(
                            &arm.expr,
                            Some(&scope.with_var(ident.clone(), expr.clone())),
                        );
                    }
                    (Pat::Num(_), Expr::Var(_ident)) => todo!(),
                    (Pat::Num(pat), Expr::Lit(LitExpr::Int(n))) if pat == n => {
                        return eval(&arm.expr, Some(scope));
                    }
                    (Pat::Num(_), Expr::Lambda(_lambda)) => todo!(),
                    (Pat::Num(_), Expr::Call(_expr, _expr1)) => todo!(),
                    (Pat::Num(_), Expr::Block(_block)) => todo!(),
                    (Pat::Num(_), Expr::Case(_expr, _vec)) => todo!(),
                    (Pat::Num(_), Expr::Tuple(_vec)) => todo!(),
                    (Pat::Num(_), Expr::Builtin(_builtin)) => todo!(),
                    (Pat::Wildcard, _) => {
                        return eval(&arm.expr, Some(scope));
                    }
                    _ => {}
                }
            }

            bail!("no arms matched")
        }
        Expr::Var(var) => Ok(scope.get(var)?.clone()),
        Expr::Tuple(exprs) => {
            if exprs.len() == 1 {
                eval(&exprs[0], Some(scope))
            } else {
                Ok(Expr::Tuple(
                    exprs
                        .iter()
                        .map(|e| eval(e, Some(scope)))
                        .collect::<Result<_, _>>()?,
                ))
            }
        }
        Expr::Builtin(builtin) => Ok(builtin.call(scope)),
        e => Ok(e.clone()),
    }
}

fn type_check(head_arg_ty: &Ty, arg: &Expr) -> bool {
    match (&head_arg_ty, arg) {
        (Ty::Atom(AtomTy::Int), Expr::Lit(LitExpr::Int(_))) => true,
        (Ty::Atom(AtomTy::Str), Expr::Lit(LitExpr::Str(_))) => true,
        (Ty::Atom(AtomTy::Bool), Expr::Lit(LitExpr::Bool(_))) => true,
        (Ty::Tuple(ty), Expr::Tuple(e)) => ty.iter().zip(e).all(|(ty, e)| type_check(ty, e)),
        (ty, Expr::Call(head, _)) => type_check(ty, head),
        (Ty::Fn(a, b), Expr::Lambda(Lambda { arg, ret, expr: _ })) => **a == arg.ty && **b == *ret,
        _ => panic!("ty: {head_arg_ty:#?}\narg: {arg:#?}"),
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
        let error = || anyhow!("variable `{i}` not found in this scope");
        self.vars
            .get(i)
            .ok_or_else(|| {
                self.parent
                    .as_ref()
                    .ok_or_else(error)
                    .and_then(|scope| scope.get(i))
            })
            .or_else(|x| x)
    }
}
