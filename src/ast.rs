pub mod expr;
pub mod ident;
pub mod stmt;
pub mod ty;

pub use ast::*;
pub use expr::{Block, Builtin, CaseArm, Expr, Lambda, LambdaArg, LitExpr, Pat};
pub use ident::Ident;
pub use stmt::Stmt;
pub use ty::{AtomTy, Ty};

peg::parser! {
    grammar ast() for str {
        pub rule parse() -> Expr = traced(<expr()>)


        rule whitespace() = quiet!{ [' ' | '\n' | '\t'] }

        rule _ = quiet! { whitespace()* }
        rule __ = whitespace()+

        rule def() -> Stmt
            = _ i:ident() _ "=" _ e:expr() _ ";" _ { Stmt { ident: i, value: e } }

        pub rule fn_args() -> Vec<LambdaArg>
            = _ e:(n:ident() _ ":" _  t:ty() _ { LambdaArg { name: n, ty: t } })+ { e }

        rule alphanumeric() = ['a'..='z' | 'A'..='Z' | '0'..='9']

        rule ident() -> Ident
            = s:$(['a'..='z' | 'A'..='Z']alphanumeric()*) { Ident::new(s).unwrap() }

        pub rule ty() -> Ty
            = ts:(_ ts:(
                i:ident() { Ty::Atom(AtomTy::from_ident(i)) }
                / "(" _ ts:ty() ** (_ ",") _ ")" { Ty::Tuple(ts) }
            ) { ts }) ++ (_ "->") {
                let mut ts = ts;

                if ts.len() == 1 {
                    ts.pop().unwrap()
                } else {
                    ts.into_iter().rev().reduce(|a, b| {
                        Ty::Fn(Box::new(b), Box::new(a))
                    })
                    .expect("expected at least 2 values")
                }
            }

        pub rule expr() -> Expr
            = _
            e:(
                call_expr()
                / simple_expr()
            )
            _
            { e }

        pub rule call_expr() -> Expr
            = l:simple_expr() _ r:expr() {
                Expr::Call(Box::new(l), Box::new(r))
            }

        pub rule simple_expr() -> Expr
            = _
            e:(
                // lambda
                lambda_expr()
                // literals
                / ("true" { Expr::Lit(LitExpr::Bool(true)) })
                / ("false" { Expr::Lit(LitExpr::Bool(false)) })
                / num_expr()
                / block_expr()
                / case_expr()
                / tuple_expr()
                // variable
                / (i:ident() { Expr::Var(i) })
            )
            { e }

        pub rule num_expr() -> Expr
            = n:num() { Expr::Lit(LitExpr::Int(n)) }

        pub rule num() -> i128
            = e:$("-"? ['0'..='9']+) { e.parse().unwrap() }

        pub rule lambda_expr() -> Expr
            = args:fn_args() _ "=>" _ t:ty() _ e:expr() { fold_lambda_expr(args, t, e) }

        pub rule block_expr() -> Expr
            = _ "{" _ stmts:def()* _ tail:expr() _ "}" { Expr::Block(Block { stmts, tail: Box::new(tail) }) }

        pub rule case_expr() -> Expr
            = _ "case" __ e:expr() _ arms:arm()* { Expr::Case(Box::new(e), arms) }

        pub rule tuple_expr() -> Expr
            = "(" _ es:expr() ++ (_ "," _) _ ")" { Expr::Tuple(es) }

        pub rule arm() -> CaseArm
            = _ p:pat() _ "=" _ e:expr() { CaseArm { pat: p, expr: Box::new(e) } }

        pub rule pat() -> Pat
            = _ "@" _ n:num() { Pat::Num(n) }
            / _ "@" _ i:ident() { Pat::Var(i) }

        // https://github.com/fasterthanlime/pegviz#integration
        rule traced<T>(e: rule<T>) -> T =
            &(input:$([_]*) {
                #[cfg(feature = "trace")]
                println!("[PEG_INPUT_START]\n{}\n[PEG_TRACE_START]", input);
            })
            e:e()? {?
                #[cfg(feature = "trace")]
                println!("[PEG_TRACE_STOP]");
                e.ok_or("")
            }
    }
}

pub fn fold_lambda_expr(mut args: Vec<LambdaArg>, t: Ty, e: Expr) -> Expr {
    let arg = args.pop().unwrap();

    let expr = args.into_iter().rev().fold(
        Lambda {
            arg,
            ret: t,
            expr: Box::new(e),
        },
        |prev, arg| Lambda {
            ret: prev.sig(),
            arg,
            expr: Box::new(Expr::Lambda(prev)),
        },
    );

    // dbg!(&expr);

    Expr::Lambda(expr)
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use super::*;
    use crate::ident;

    #[track_caller]
    fn assert_parse<T: Debug + PartialEq>(
        f: fn(&str) -> Result<T, peg::error::ParseError<<str as peg::Parse>::PositionRepr>>,
        input: &str,
        output: T,
    ) {
        let o = f(input);

        #[cfg(not(feature = "trace"))]
        assert_eq!(o, Ok(output))
    }

    #[test]
    fn ty() {
        assert_parse(ast::ty, "ty", Ty::Atom(AtomTy::Named(ident!("ty"))));
        assert_parse(
            ast::ty,
            "(ty, ty)",
            Ty::Tuple(vec![
                Ty::Atom(AtomTy::Named(ident!("ty"))),
                Ty::Atom(AtomTy::Named(ident!("ty"))),
            ]),
        );
        assert_parse(
            ast::ty,
            "( ty ,ty )",
            Ty::Tuple(vec![
                Ty::Atom(AtomTy::Named(ident!("ty"))),
                Ty::Atom(AtomTy::Named(ident!("ty"))),
            ]),
        );
        assert_parse(
            ast::ty,
            "( ty ,  ty )",
            Ty::Tuple(vec![
                Ty::Atom(AtomTy::Named(ident!("ty"))),
                Ty::Atom(AtomTy::Named(ident!("ty"))),
            ]),
        );
        assert_parse(
            ast::ty,
            "( ty,   ty  ,ty )",
            Ty::Tuple(vec![
                Ty::Atom(AtomTy::Named(ident!("ty"))),
                Ty::Atom(AtomTy::Named(ident!("ty"))),
                Ty::Atom(AtomTy::Named(ident!("ty"))),
            ]),
        );
        assert_parse(
            ast::ty,
            "int -> int",
            Ty::Fn(
                Box::new(Ty::Atom(AtomTy::Int)),
                Box::new(Ty::Atom(AtomTy::Int)),
            ),
        );
        assert_parse(
            ast::ty,
            "( ty  , ty,  ty ) -> int",
            Ty::Fn(
                Box::new(Ty::Tuple(vec![
                    Ty::Atom(AtomTy::Named(ident!("ty"))),
                    Ty::Atom(AtomTy::Named(ident!("ty"))),
                    Ty::Atom(AtomTy::Named(ident!("ty"))),
                ])),
                Box::new(Ty::Atom(AtomTy::Int)),
            ),
        );
    }

    #[test]
    fn fn_args() {
        assert_parse(
            ast::fn_args,
            "n: int",
            vec![LambdaArg {
                name: ident!("n"),
                ty: Ty::Atom(AtomTy::Int),
            }],
        );
        assert_parse(
            ast::fn_args,
            "n: int",
            vec![LambdaArg {
                name: ident!("n"),
                ty: Ty::Atom(AtomTy::Int),
            }],
        );
        assert_parse(
            ast::fn_args,
            "n: int -> int",
            vec![LambdaArg {
                name: ident!("n"),
                ty: Ty::Fn(
                    Box::new(Ty::Atom(AtomTy::Int)),
                    Box::new(Ty::Atom(AtomTy::Int)),
                ),
            }],
        );
        assert_parse(
            ast::fn_args,
            "n: int m: int",
            vec![
                LambdaArg {
                    name: ident!("n"),
                    ty: Ty::Atom(AtomTy::Int),
                },
                LambdaArg {
                    name: ident!("m"),
                    ty: Ty::Atom(AtomTy::Int),
                },
            ],
        );
        assert_parse(
            ast::fn_args,
            "n: int -> (bool, str -> int) m: int",
            vec![
                LambdaArg {
                    name: ident!("n"),
                    ty: Ty::Fn(
                        Box::new(Ty::Atom(AtomTy::Int)),
                        Box::new(Ty::Tuple(vec![
                            Ty::Atom(AtomTy::Bool),
                            Ty::Fn(
                                Box::new(Ty::Atom(AtomTy::Str)),
                                Box::new(Ty::Atom(AtomTy::Int)),
                            ),
                        ])),
                    ),
                },
                LambdaArg {
                    name: ident!("m"),
                    ty: Ty::Atom(AtomTy::Int),
                },
            ],
        );
    }

    #[test]
    fn fold_lambda_args() {
        let args = vec![
            LambdaArg {
                name: ident!("a"),
                ty: Ty::Atom(AtomTy::Named(ident!("a"))),
            },
            LambdaArg {
                name: ident!("b"),
                ty: Ty::Atom(AtomTy::Named(ident!("b"))),
            },
            LambdaArg {
                name: ident!("c"),
                ty: Ty::Atom(AtomTy::Named(ident!("c"))),
            },
        ];

        assert_parse(
            ast::lambda_expr,
            "a:a=>b->c->bool b:b=>c->bool c:c=>bool true",
            fold_lambda_expr(args, Ty::Atom(AtomTy::Bool), Expr::Lit(LitExpr::Bool(true))),
        );
    }

    #[test]
    fn lambda_expr() {
        assert_parse(
            ast::lambda_expr,
            "n: int => int n",
            Expr::Lambda(Lambda {
                arg: LambdaArg {
                    name: ident!("n"),
                    ty: Ty::Atom(AtomTy::Int),
                },
                expr: Box::new(Expr::Var(ident!("n"))),
                ret: Ty::Atom(AtomTy::Int),
            }),
        );
        assert_parse(
            ast::lambda_expr,
            "a: a b: b c: c => int 1",
            Expr::Lambda(Lambda {
                arg: LambdaArg {
                    name: ident!("a"),
                    ty: Ty::Atom(AtomTy::Named(ident!("a"))),
                },
                expr: Box::new(Expr::Lambda(Lambda {
                    arg: LambdaArg {
                        name: ident!("b"),
                        ty: Ty::Atom(AtomTy::Named(ident!("b"))),
                    },
                    expr: Box::new(Expr::Lambda(Lambda {
                        arg: LambdaArg {
                            name: ident!("c"),
                            ty: Ty::Atom(AtomTy::Named(ident!("c"))),
                        },
                        expr: Box::new(Expr::Lit(LitExpr::Int(1))),
                        ret: Ty::Atom(AtomTy::Int),
                    })),
                    ret: Ty::Fn(
                        Box::new(Ty::Atom(AtomTy::Named(ident!("c")))),
                        Box::new(Ty::Atom(AtomTy::Int)),
                    ),
                })),
                ret: Ty::Fn(
                    Box::new(Ty::Atom(AtomTy::Named(ident!("b")))),
                    Box::new(Ty::Fn(
                        Box::new(Ty::Atom(AtomTy::Named(ident!("c")))),
                        Box::new(Ty::Atom(AtomTy::Int)),
                    )),
                ),
            }),
        );
    }
}
