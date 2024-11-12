use anyhow::{anyhow, bail, ensure};
use itertools::Itertools;
use tracing::{debug, instrument};

use crate::ast::{AtomTy, Block, CaseArm, Expr, FnTy, Ident, Lambda, LitExpr, Pat, Stmt, Ty};

#[instrument(name = "n", skip_all)]
pub fn normalize(expr: &Expr) -> anyhow::Result<Expr> {
    debug!(%expr);

    // std::thread::sleep(std::time::Duration::from_secs_f32(0.5));

    // dbg!(scope);

    match expr {
        Expr::Call(expr, arg) => {
            let expr = normalize(expr)?;

            debug!(%expr, %arg);

            match expr {
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

                            let expr = substitute(*lambda.expr, &lambda.arg.name, &arg)?;

                            let expr = Expr::Call(Box::new(expr), tail.clone());

                            normalize(&expr)?
                        }
                        _ => {
                            let arg = normalize(arg)?;

                            type_check(&lambda.arg.ty, &arg)?;

                            let expr = substitute(*lambda.expr, &lambda.arg.name, &arg)?;

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
                Expr::Tuple(tuple) => match &**arg {
                    Expr::Lit(LitExpr::Int(n)) => tuple
                        .get(*n as usize)
                        .ok_or(anyhow!(
                            "cannot index into tuple of arity {} with index {n}",
                            tuple.len()
                        ))
                        .cloned(),
                    _ => bail!("attempted to call a tuple"),
                },
                Expr::DefinedSymbol(symbol, ty) => Ok(Expr::Call(
                    Box::new(Expr::DefinedSymbol(symbol, ty)),
                    arg.clone(),
                )),
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
                        s.value = substitute(s.value, &stmt.ident, &stmt.value)?;
                        Ok(s)
                    })
                    .collect::<anyhow::Result<_>>()?;
                block.tail = Box::new(substitute(*block.tail, &stmt.ident, &stmt.value)?);

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
                        return normalize(&substitute(*arm.expr.clone(), ident, &expr)?);
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
            let mut lambda = lambda.clone();

            lambda.expr = Box::new(substitute(
                *lambda.expr.clone(),
                &lambda.arg.name,
                &Expr::DefinedSymbol(lambda.arg.name.clone(), lambda.arg.ty.clone()),
            )?);

            type_check(&lambda.ret, &lambda.expr)?;

            Ok(Expr::Lambda(lambda.clone()))
        }
        e => Ok(e.clone()),
    }
}

#[instrument(name = "s", skip_all)]
#[must_use]
pub fn substitute(expr: Expr, symbol: &Ident, value: &Expr) -> anyhow::Result<Expr> {
    debug!(%symbol, %value, into=%expr);

    match expr {
        Expr::Symbol(ident) => {
            if ident == *symbol {
                debug!("found symbol {ident}, substituting with {value}");
                Ok(value.clone())
            } else {
                Ok(Expr::Symbol(ident.clone()))
            }
        }
        Expr::DefinedSymbol(ident, ty) => {
            if ident == *symbol {
                let value_ty = type_of(value)?;
                ensure!(
                    ty == value_ty,
                    "symbol `{ident}` is of type `{ty}`, but attempted \
                    to substitute a value of type `{value_ty}` (`{value}`)"
                );
                debug!("found symbol {ident}, substituting with {value}");
                Ok(value.clone())
            } else {
                Ok(Expr::DefinedSymbol(ident.clone(), ty.clone()))
            }
        }
        Expr::Lambda(lambda) => {
            if lambda.arg.name != *symbol {
                Ok(Expr::Lambda(Lambda {
                    arg: lambda.arg.clone(),
                    ret: lambda.ret.clone(),
                    expr: Box::new(substitute(*lambda.expr, symbol, value)?),
                }))
            } else {
                Ok(Expr::Lambda(lambda.clone()))
            }
        }
        Expr::Call(caller, arg) => Ok(Expr::Call(
            Box::new(substitute(*caller, symbol, value)?),
            Box::new(substitute(*arg, symbol, value)?),
        )),
        Expr::Block(block) => Ok(Expr::Block(Block {
            stmts: block
                .stmts
                .into_iter()
                .map(|s| {
                    Ok(Stmt {
                        ident: s.ident.clone(),
                        value: substitute(s.value, symbol, value)?,
                    })
                })
                .collect::<anyhow::Result<_>>()?,
            tail: Box::new(substitute(*block.tail, symbol, value)?),
        })),
        Expr::Case(expr, arms) => Ok(Expr::Case(
            Box::new(substitute(*expr, symbol, value)?),
            arms.into_iter()
                .map(|arm| {
                    Ok(CaseArm {
                        pat: arm.pat.clone(),
                        expr: Box::new(substitute(*arm.expr, symbol, value)?),
                    })
                })
                .collect::<anyhow::Result<_>>()?,
        )),
        Expr::Tuple(tuple) => Ok(Expr::Tuple(
            tuple
                .into_iter()
                .map(|t| substitute(t, symbol, value))
                .collect::<Result<_, _>>()?,
        )),
        _ => Ok(expr.clone()),
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
        (Ty::Fn(f), expr) => {
            let expr_ty = type_of(expr)?;

            if f.0 == expr_ty {
                Ok(())
            } else {
                Err(anyhow!(
                    "attempted to use a value of type `{expr_ty}` (`{expr}`) as a value of type `{ty}`"
                ))
            }
        }
        (_ty, _expr) => Ok(()),
    }
}

// NOTE: Assumes fully substituted expresssions
#[instrument(name = "o", skip_all)]
pub fn type_of(expr: &Expr) -> anyhow::Result<Ty> {
    debug!(%expr);

    let ty = match expr {
        Expr::Symbol(ident) => Err(anyhow!("undefined symbol `{ident}`")),
        Expr::DefinedSymbol(_, ty) => Ok(ty.clone()),
        Expr::Lit(lit_expr) => Ok(lit_expr.ty()),
        Expr::Lambda(_lambda) => todo!(),
        Expr::Call(expr, arg) => {
            let expr_ty = type_of(expr)?;

            match (expr_ty, &**arg) {
                (Ty::Tuple(tuple), Expr::Lit(LitExpr::Int(n))) => tuple
                    .get(*n as usize)
                    .ok_or(anyhow!(
                        "cannot index into tuple of arity {} with index {n}",
                        tuple.len()
                    ))
                    .cloned(),
                // (expr_ty, _) => Err(anyhow!(
                //     "attempted to call an expression of type `{expr_ty}` \
                //     (`{expr}`) with a value of type `{arg_ty}` (`{arg}`)"
                // )),
                (expr_ty, arg) => match (expr_ty, type_of(arg)?) {
                    (Ty::Fn(fn_ty), arg_ty) if fn_ty.0 == arg_ty => Ok(fn_ty.1),
                    (expr_ty, arg_ty) => Ok(Ty::Fn(Box::new(FnTy(expr_ty, arg_ty)))),
                },
            }
        }
        Expr::Block(_block) => todo!(),
        Expr::Case(_expr, _vec) => todo!(),
        Expr::Tuple(tuple) => {
            let mut tuple = tuple.iter().map(type_of).collect::<Result<Vec<_>, _>>()?;

            Ok(if tuple.len() == 1 {
                tuple.pop().unwrap()
            } else {
                Ty::Tuple(tuple)
            })
        }
        Expr::Builtin(builtin) => Ok(Ty::Fn(Box::new(builtin.sig()))),
    }?;

    debug!(%ty);

    Ok(ty)
}
