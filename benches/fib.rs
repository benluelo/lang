use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lang::{
    ast::{self, Expr},
    program::normalize,
};

fn bench_fib(_c: &mut Criterion) {
    let mut c = Criterion::default().sample_size(10);

    c.bench_function("fib 10", |b| {
        b.iter_with_setup(
            || setup(10),
            |program| black_box(normalize(&program).unwrap()),
        )
    });

    c.bench_function("fib 20", |b| {
        b.iter_with_setup(
            || setup(20),
            |program| black_box(normalize(&program).unwrap()),
        )
    });

    c.bench_function("fib 25", |b| {
        b.iter_with_setup(
            || setup(25),
            |program| black_box(normalize(&program).unwrap()),
        )
    });
}

fn setup(n: i128) -> Expr {
    let expr = "
{
    fib = n: int => int case n
        @ 0 = 0
        @ 1 = 1
        @ n = add (fib (sub n 1)) (fib (sub n 2));

    fib
}
    ";

    let program = Expr::Call(
        Box::new(ast::parse(expr).unwrap()),
        Box::new(Expr::Lit(ast::LitExpr::Int(n))),
    );

    // let scope = Rc::new(Scope::new(
    //     [
    //         (ident!("add"), lang::builtins::add()),
    //         // (ident!("sub"), lang::builtins::sub()),
    //         // (ident!("mul"), lang::builtins::mul()),
    //     ]
    //     .into_iter()
    //     .collect(),
    // ));

    let out = normalize(&program).unwrap();

    dbg!(out);

    program
}

criterion_group!(benches, bench_fib);

criterion_main!(benches);
