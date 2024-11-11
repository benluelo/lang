use anyhow::{anyhow, bail, ensure};
use itertools::Itertools;
use tracing::{debug, instrument};

use crate::ast::{AtomTy, Block, CaseArm, Expr, Ident, Lambda, LitExpr, Pat, Stmt, Ty};

#[instrument(name = "n", skip_all)]
pub fn normalize(expr: &Expr) -> anyhow::Result<Expr> {
    debug!(%expr);

    // std::thread::sleep(std::time::Duration::from_secs_f32(0.5));

    // dbg!(scope);

    match expr {
        Expr::Call(expr, arg) => {
            let caller = normalize(expr)?;
            debug!(caller = %expr, normalized_caller = %caller, callee = %arg);

            match caller {
                Expr::Lit(lit_expr) => bail!(
                    "attempted to call a literal value of type `{}` \
                    (value `{lit_expr}`) with value `{arg}`",
                    lit_expr.ty()
                ),
                Expr::Lambda(lambda) => {
                    debug!(%lambda, %arg);

                    let sig = lambda.sig();

                    let output = match &**arg {
                        Expr::Call(arg, tail) => {
                            let arg = normalize(arg)?;

                            type_check(&lambda.arg.ty, &arg)?;

                            let expr = substitute(*lambda.expr, &lambda.arg.name, &arg);

                            let expr = Expr::Call(Box::new(expr), tail.clone());

                            normalize(&expr)?
                        }
                        _ => {
                            let arg = normalize(arg)?;

                            type_check(&lambda.arg.ty, &arg)?;

                            let expr = substitute(*lambda.expr, &lambda.arg.name, &arg);

                            normalize(&expr)?
                        }
                    };

                    type_check(&sig.1, &output)?;

                    Ok(output)
                }
                Expr::Call(caller, caller2) => {
                    let expr = Expr::Call(caller, Box::new(Expr::Call(caller2, arg.clone())));

                    normalize(&expr)
                }
                Expr::Block(_block) => todo!(),
                Expr::Case(_expr, _vec) => todo!(),
                Expr::Builtin(builtin) => {
                    debug!(%builtin, %arg);

                    let output = match &**arg {
                        Expr::Call(arg, tail) => {
                            let arg = normalize(arg)?;

                            let expr = Expr::Call(
                                Box::new(normalize(&builtin.call(arg.clone())?)?),
                                tail.clone(),
                            );

                            normalize(&expr)?
                        }
                        _ => {
                            let arg = normalize(arg)?;

                            normalize(&builtin.call(arg.clone())?)?
                        }
                    };

                    type_check(&builtin.sig().1, &output)?;

                    debug!(%output);

                    Ok(output)
                }
                Expr::Tuple(_tuple) => bail!("attempted to call a tuple"),
                _ => todo!(),
            }
        }
        Expr::Block(block) => {
            let mut block = block.clone();

            block
                .stmts
                .iter()
                .map(|s| (&s.ident, &s.value))
                .into_group_map()
                .iter()
                .filter(|x| x.1.len() > 1)
                .map(|s| format!("duplicate definition of `{}`", s.0))
                .reduce(|a, b| format!("{a}\n{b}"))
                .map_or(Ok(()), |e| Err(anyhow!(e)))?;

            for stmt in &block.stmts.clone() {
                // let Some(stmt) = block.stmts.pop_front() else {
                //     break;
                // };

                // TODO: Improve this check to recurse into subexpressions
                if Expr::Symbol(stmt.ident.clone()) == stmt.value {
                    bail!("recursive definition of `{}`", stmt.ident)
                }

                debug!(%block, %stmt);

                block.stmts = block
                    .stmts
                    .into_iter()
                    .map(|mut s| {
                        s.value = substitute(s.value, &stmt.ident, &stmt.value);
                        s
                    })
                    .collect();
                block.tail = Box::new(substitute(*block.tail, &stmt.ident, &stmt.value));

                // block.stmts.push_back(stmt);
            }

            normalize(&block.tail)
        }
        Expr::Case(expr, arms) => {
            let expr = normalize(expr)?;

            for arm in arms {
                debug!(%arm);

                match (&arm.pat, &expr) {
                    (Pat::Symbol(ident), _) => {
                        return normalize(&substitute(*arm.expr.clone(), ident, &expr));
                    }
                    (Pat::Num(_), Expr::Symbol(_ident)) => todo!(),
                    (Pat::Num(pat), Expr::Lit(LitExpr::Int(n))) if pat == n => {
                        return normalize(&arm.expr);
                    }
                    (Pat::Num(_), Expr::Lambda(_lambda)) => todo!(),
                    (Pat::Num(_), Expr::Call(_expr, _expr1)) => todo!(),
                    (Pat::Num(_), Expr::Block(_block)) => todo!(),
                    (Pat::Num(_), Expr::Case(_expr, _vec)) => todo!(),
                    (Pat::Num(_), Expr::Tuple(_vec)) => todo!(),
                    (Pat::Num(_), Expr::Builtin(_builtin)) => todo!(),
                    (Pat::Wildcard, _) => {
                        return normalize(&arm.expr);
                    }
                    _ => {}
                }
            }

            bail!("no arms matched")
        }
        Expr::Tuple(exprs) => {
            if exprs.len() == 1 {
                normalize(&exprs[0])
            } else {
                Ok(Expr::Tuple(
                    exprs.iter().map(normalize).collect::<Result<_, _>>()?,
                ))
            }
        }
        // if we hit a symbol that hasn't been substituted, it's undefined
        Expr::Symbol(ident) => {
            bail!("undefined symbol `{ident}`")
        }
        Expr::Lambda(lambda) => {
            type_check(&lambda.ret, &lambda.expr)?;

            Ok(Expr::Lambda(lambda.clone()))
        }
        e => Ok(e.clone()),
    }
}

#[instrument(name = "s", skip_all)]
pub fn substitute(expr: Expr, symbol: &Ident, value: &Expr) -> Expr {
    debug!(%symbol, %value, into=%expr);

    match expr {
        Expr::Symbol(ident) => {
            if ident == *symbol {
                debug!("found symbol {ident}, substituting with {value}");
                value.clone()
            } else {
                Expr::Symbol(ident.clone())
            }
        }
        Expr::Lambda(lambda) => {
            if lambda.arg.name != *symbol {
                Expr::Lambda(Lambda {
                    arg: lambda.arg.clone(),
                    ret: lambda.ret.clone(),
                    expr: Box::new(substitute(*lambda.expr, symbol, value)),
                })
            } else {
                Expr::Lambda(lambda.clone())
            }
        }
        Expr::Call(caller, arg) => Expr::Call(
            Box::new(substitute(*caller, symbol, value)),
            Box::new(substitute(*arg, symbol, value)),
        ),
        Expr::Block(block) => Expr::Block(Block {
            stmts: block
                .stmts
                .into_iter()
                .map(|s| Stmt {
                    ident: s.ident.clone(),
                    value: substitute(s.value, symbol, value),
                })
                .collect(),
            tail: Box::new(substitute(*block.tail, symbol, value)),
        }),
        Expr::Case(expr, arms) => Expr::Case(
            Box::new(substitute(*expr, symbol, value)),
            // TODO: ensure shadowing rules apply correctly
            arms.into_iter()
                .map(|arm| CaseArm {
                    pat: arm.pat.clone(),
                    expr: Box::new(substitute(*arm.expr, symbol, value)),
                })
                .collect(),
        ),
        Expr::Tuple(tuple) => Expr::Tuple(
            tuple
                .into_iter()
                .map(|t| substitute(t, symbol, value))
                .collect(),
        ),
        _ => expr.clone(),
    }
}

#[instrument(name = "t", skip_all)]
pub fn type_check(ty: &Ty, expr: &Expr) -> anyhow::Result<()> {
    debug!(%ty, %expr);

    match (&ty, expr) {
        (Ty::Atom(AtomTy::Int), Expr::Lit(LitExpr::Int(_))) => Ok(()),
        (Ty::Atom(AtomTy::Bool), Expr::Lit(LitExpr::Bool(_))) => Ok(()),
        (ty, Expr::Block(block)) => type_check(ty, &block.tail),
        (Ty::Tuple(ty), Expr::Tuple(expr)) => {
            ensure!(
                ty.len() == expr.len(),
                "attempted to use a tuple of length {} as a tuple of length {}",
                expr.len(),
                ty.len()
            );
            ty.iter()
                .zip(expr)
                .try_for_each(|(ty, expr)| type_check(ty, expr))
        }
        // (ty, Expr::Call(head, _)) => type_check(ty, head),
        (Ty::Fn(f), Expr::Lambda(lambda)) => {
            if lambda.sig() == **f {
                Ok(())
            } else {
                Err(anyhow!(
                    "attempted to use a lambda of type `{lambda}` as a lambda of type `{f}`"
                ))
            }
        }
        (Ty::Fn(f), Expr::Builtin(builtin)) => {
            let sig = builtin.sig();
            if sig == **f {
                Ok(())
            } else {
                Err(anyhow!(
                    "attempted to use builtin `{}` with type `{sig}` as an expression of type `{f}`",
                    builtin.name()
                ))
            }
        }
        (ty, expr) => {
            let expr_ty = type_of(expr)?;

            if **ty == expr_ty {
                Ok(())
            } else {
                Err(anyhow!(
                    "attempted to use a value of type `{expr}` as a value of type `{ty}`"
                ))
            }
        }
    }
}

// NOTE: Assumes fully substituted expresssions
pub fn type_of(expr: &Expr) -> anyhow::Result<Ty> {
    match expr {
        Expr::Symbol(ident) => Err(anyhow!("undefined symbol `{ident}`")),
        Expr::Lit(lit_expr) => Ok(lit_expr.ty()),
        Expr::Lambda(lambda) => Ok(todo!()),
        Expr::Call(expr, arg) => {
            let expr_ty = type_of(expr)?;
            let arg_ty = type_of(arg)?;

            dbg!(expr_ty, arg_ty);

            todo!();
        }
        Expr::Block(block) => Ok(todo!()),
        Expr::Case(expr, vec) => Ok(todo!()),
        Expr::Tuple(vec) => vec
            .iter()
            .map(type_of)
            .collect::<Result<_, _>>()
            .map(Ty::Tuple),
        Expr::Builtin(builtin) => Ok(Ty::Fn(Box::new(builtin.sig()))),
    }
}
