#![feature(trait_alias)]
#![warn(clippy::unwrap_in_result)]

pub mod ast;

pub mod program;

pub mod builtins {
    // macro_rules! builtin {
    //     (pub fn $f:ident($scope:ident: &Scope, $($arg:ident: $ty:ty),+) -> $ret:ty $body:block) => {
    //         pub fn $f() -> Expr {
    //             fold_lambda_expr(
    //                 vec![$(
    //                     LambdaArg {
    //                         name: Ident::new_static(stringify!($arg)),
    //                         ty: Ty::Atom(AtomTy::from_ident_static(stringify!($ty))),
    //                     }
    //                 ),+],
    //                 Ty::Atom(AtomTy::from_ident_static(stringify!($ret))),
    //                 Expr::Builtin(Builtin::new(stringify!($f), |$scope| $body)),
    //             )
    //         }
    //     };
    // }

    use anyhow::bail;

    use crate::{
        ast::{AtomTy, Block, Builtin, Expr, FnTy, LitExpr, Stmt, Ty},
        ident,
    };

    pub fn add() -> Expr {
        Expr::Builtin(Builtin::new(
            "add2",
            Ty::Atom(AtomTy::Int),
            Ty::Fn(Box::new(FnTy(Ty::Atom(AtomTy::Int), Ty::Atom(AtomTy::Int)))),
            |e| match e {
                Expr::Lit(LitExpr::Int(a)) => Ok(Expr::Builtin(Builtin::new(
                    "add1",
                    Ty::Atom(AtomTy::Int),
                    Ty::Atom(AtomTy::Int),
                    move |e| match e {
                        Expr::Lit(LitExpr::Int(b)) => Ok(Expr::Lit(LitExpr::Int(a + b))),
                        _ => bail!("expected int, found {e}"),
                    },
                ))),
                _ => bail!("expected int, found {e}"),
            },
        ))
    }

    pub fn sub() -> Expr {
        Expr::Builtin(Builtin::new(
            "sub2",
            Ty::Atom(AtomTy::Int),
            Ty::Fn(Box::new(FnTy(Ty::Atom(AtomTy::Int), Ty::Atom(AtomTy::Int)))),
            |e| match e {
                Expr::Lit(LitExpr::Int(a)) => Ok(Expr::Builtin(Builtin::new(
                    "sub1",
                    Ty::Atom(AtomTy::Int),
                    Ty::Atom(AtomTy::Int),
                    move |e| match e {
                        Expr::Lit(LitExpr::Int(b)) => Ok(Expr::Lit(LitExpr::Int(a - b))),
                        _ => bail!("expected int, found {e}"),
                    },
                ))),
                _ => bail!("expected int, found {e}"),
            },
        ))
    }

    // builtin! {
    //     pub fn add(scope: &Scope, a: int, b: int) -> int {
    //         let a = scope.get(&ident!("a")).unwrap();
    //         let b = scope.get(&ident!("b")).unwrap();

    //         let (Expr::Lit(LitExpr::Int(a)), Expr::Lit(LitExpr::Int(b))) = (a, b) else {
    //             bail!("cannot add `{a}` and `{b}`")
    //         };

    //         Ok(Expr::Lit(LitExpr::Int(a + b)))
    //     }
    // }

    // builtin! {
    //     pub fn sub(scope: &Scope, a: int, b: int) -> int {
    //         let a = scope.get(&ident!("a")).unwrap();
    //         let b = scope.get(&ident!("b")).unwrap();

    //         let (Expr::Lit(LitExpr::Int(a)), Expr::Lit(LitExpr::Int(b))) = (a, b) else {
    //             bail!("cannot sub `{a}` and `{b}`")
    //         };

    //         Ok(Expr::Lit(LitExpr::Int(a - b)))
    //     }
    // }

    // builtin! {
    //     pub fn mul(scope: &Scope, a: int, b: int) -> int {
    //         let a = scope.get(&ident!("a")).unwrap();
    //         let b = scope.get(&ident!("b")).unwrap();

    //         let (Expr::Lit(LitExpr::Int(a)), Expr::Lit(LitExpr::Int(b))) = (a, b) else {
    //             bail!("cannot mul `{a}` and `{b}`")
    //         };

    //         Ok(Expr::Lit(LitExpr::Int(a * b)))
    //     }
    // }

    pub fn core(expr: Expr) -> Expr {
        Expr::Block(Block {
            stmts: [
                Stmt {
                    ident: ident!("add"),
                    value: add(),
                },
                // Stmt {
                //     ident: ident!("sub"),
                //     value: builtins::sub(),
                // },
                // Stmt {
                //     ident: ident!("mul"),
                //     value: builtins::mul(),
                // },
            ]
            .into(),
            tail: Box::new(expr),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Expr, program::normalize};

    #[test]
    fn tests() {
        let expr = normalize(&builtins::core(
            ast::parse("{ idx = 1; (ns: (int, int) a: int => int add a (ns idx)) (1, 2) 1000 }")
                .unwrap(),
        ))
        .unwrap();

        assert_eq!(expr, Expr::Lit(ast::LitExpr::Int(1002)));
    }
}
