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
            (ident!("mul"), builtins::mul()),
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
    macro_rules! builtin {
        (pub fn $f:ident($scope:ident: &Scope, $($arg:ident: $ty:ty),+) -> $ret:ty $body:block) => {
            pub fn $f() -> Expr {
                fold_lambda_expr(
                    vec![$(
                        LambdaArg {
                            name: Ident::new_static(stringify!($arg)),
                            ty: Ty::Atom(AtomTy::from_ident_static(stringify!($ty))),
                        }
                    ),+],
                    Ty::Atom(AtomTy::from_ident_static(stringify!($ret))),
                    Expr::Builtin(Builtin::new(stringify!($f), |$scope| $body)),
                )
            }
        };
    }

    use anyhow::bail;

    use crate::{
        ast::{fold_lambda_expr, AtomTy, Builtin, Expr, Ident, Lambda, LambdaArg, LitExpr, Ty},
        ident,
    };

    builtin! {
        pub fn add(scope: &Scope, a: int, b: int) -> int {
            let a = scope.get(&ident!("a")).unwrap();
            let b = scope.get(&ident!("b")).unwrap();

            let (Expr::Lit(LitExpr::Int(a)), Expr::Lit(LitExpr::Int(b))) = (a, b) else {
                bail!("cannot add `{a}` and `{b}`")
            };

            Ok(Expr::Lit(LitExpr::Int(a + b)))
        }
    }

    builtin! {
        pub fn sub(scope: &Scope, a: int, b: int) -> int {
            let a = scope.get(&ident!("a")).unwrap();
            let b = scope.get(&ident!("b")).unwrap();

            let (Expr::Lit(LitExpr::Int(a)), Expr::Lit(LitExpr::Int(b))) = (a, b) else {
                bail!("cannot sub `{a}` and `{b}`")
            };

            Ok(Expr::Lit(LitExpr::Int(a - b)))
        }
    }

    builtin! {
        pub fn mul(scope: &Scope, a: int, b: int) -> int {
            let a = scope.get(&ident!("a")).unwrap();
            let b = scope.get(&ident!("b")).unwrap();

            let (Expr::Lit(LitExpr::Int(a)), Expr::Lit(LitExpr::Int(b))) = (a, b) else {
                bail!("cannot mul `{a}` and `{b}`")
            };

            Ok(Expr::Lit(LitExpr::Int(a * b)))
        }
    }
}
