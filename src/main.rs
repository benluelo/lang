#![feature(trait_alias)]
#![warn(clippy::unwrap_in_result)]

use crate::program::{eval, Scope};

pub mod ast;

pub mod program;

fn main() -> anyhow::Result<()> {
    let input = "
entry = n: uint => uint {
x=1;1
};";
    let input = "
entry = n: uint => uint {
    fib n m { x = 23; 1 } a: bool b: str => int a
};";
    let input = "{
    fib = n: uint => uint case n
        @ 0 = 0
        @ 1 = 1
        @ n = add { fib { sub n 1 } } { { sub n 2 } };

    n: uint => uint {
        fib n
    }
}
";
    let input = "(int, int)";

    let raw = std::env::args().nth(1).unwrap();

    // let input = "case n @ n = add { sub n 1 } { sub n 2 }";
    // let input = "case n @ n = n @ 1 = m n { n } { e }";
    // let input = "case n
    // @ 0 = 0
    // @ 1 = 1
    // @ n = add { sub n 1 } { sub n 2 }";
    // let input = "entry = n: uint m: uint => uint { 1 }";
    // let input = "n: uint";

    let res = ast::parse(&raw);

    println!("raw: {raw}");

    println!("{res:#?}");

    let program = res.unwrap();

    println!("in:  {program}");

    let scope = Scope::new(
        [
            (ident!("add"), builtins::add()),
            (ident!("sub"), builtins::sub()),
        ]
        .into_iter()
        .collect(),
    );

    dbg!(&scope);

    let out = eval(&program, &scope)?;

    println!("out: {out}");
    Ok(())
}

pub mod builtins {
    use crate::{
        ast::{AtomTy, Builtin, Expr, Lambda, LambdaArg, LitExpr, Ty},
        ident,
    };

    pub fn add() -> Expr {
        Expr::Lambda(Lambda {
            arg: LambdaArg {
                name: ident!("a"),
                ty: Ty::Atom(AtomTy::Int),
            },
            expr: Box::new(Expr::Lambda(Lambda {
                arg: LambdaArg {
                    name: ident!("b"),
                    ty: Ty::Atom(AtomTy::Int),
                },
                expr: Box::new(Expr::Builtin(Builtin::new(|scope| {
                    // dbg!(&scope);

                    let a = scope.get(&ident!("a")).unwrap();
                    let b = scope.get(&ident!("b")).unwrap();

                    let (Expr::Lit(LitExpr::Int(a)), Expr::Lit(LitExpr::Int(b))) = (a, b) else {
                        panic!("cannot add `{a}` and `{b}`")
                    };

                    Expr::Lit(LitExpr::Int(a + b))
                }))),
                ret: Ty::Atom(AtomTy::Int),
            })),
            ret: Ty::Fn(
                Box::new(Ty::Atom(AtomTy::Int)),
                Box::new(Ty::Atom(AtomTy::Int)),
            ),
        })
    }

    pub fn sub() -> Expr {
        Expr::Lambda(Lambda {
            arg: LambdaArg {
                name: ident!("a"),
                ty: Ty::Atom(AtomTy::Int),
            },
            expr: Box::new(Expr::Lambda(Lambda {
                arg: LambdaArg {
                    name: ident!("b"),
                    ty: Ty::Atom(AtomTy::Int),
                },
                expr: Box::new(Expr::Builtin(Builtin::new(|scope| {
                    // dbg!(&scope);

                    let a = scope.get(&ident!("a")).unwrap();
                    let b = scope.get(&ident!("b")).unwrap();

                    let (Expr::Lit(LitExpr::Int(a)), Expr::Lit(LitExpr::Int(b))) = (a, b) else {
                        panic!("cannot sub `{a}` and `{b}`")
                    };

                    Expr::Lit(LitExpr::Int(a - b))
                }))),
                ret: Ty::Atom(AtomTy::Int),
            })),
            ret: Ty::Fn(
                Box::new(Ty::Atom(AtomTy::Int)),
                Box::new(Ty::Atom(AtomTy::Int)),
            ),
        })
    }
}

macro_rules! builtin {
    () => {};
}
