#![feature(trait_alias)]
#![warn(clippy::unwrap_in_result)]

use anyhow::bail;
use lang::{ast, builtins, program::normalize};
use tracing_subscriber::EnvFilter;

fn main() -> anyhow::Result<()> {
    #[cfg(not(feature = "tracing-off"))]
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .without_time()
        .with_target(false)
        .init();

    //     let input = "
    // entry = n: uint => uint {
    // x=1;1
    // };";
    //     let input = "
    // entry = n: uint => uint {
    //     fib n m { x = 23; 1 } a: bool b: str => int a
    // };";
    //     let input = "{
    //     fib = n: uint => uint case n
    //         @ 0 = 0
    //         @ 1 = 1
    //         @ n = add { fib { sub n 1 } } { { sub n 2 } };

    //     n: uint => uint {
    //         fib n
    //     }
    // }
    // ";
    //     let input = "(int, int)";

    let raw = std::env::args().nth(1).unwrap();

    match &*raw {
        "eval" => {
            // let input = "case n @ n = add { sub n 1 } { sub n 2 }";
            // let input = "case n @ n = n @ 1 = m n { n } { e }";
            // let input = "case n
            // @ 0 = 0
            // @ 1 = 1
            // @ n = add { sub n 1 } { sub n 2 }";
            // let input = "entry = n: uint m: uint => uint { 1 }";
            // let input = "n: uint";

            let res = ast::parse(&std::env::args().nth(2).unwrap());

            println!("{res:#?}");

            let program = res.unwrap();

            println!("in: {program}");

            let program = builtins::core(program);

            println!("full: {program}");

            let out = normalize(&program)?;

            println!("out: {out}");

            Ok(())
        }
        "run" => {
            let path = std::env::args().nth(2).unwrap();

            let file = std::fs::read_to_string(path)?;

            println!("raw: {file}");

            let program = builtins::core(ast::parse(&file)?);

            let out = normalize(&program)?;

            println!("out: {out}");
            Ok(())
        }
        _ => bail!("unknown command `{raw}`"),
    }
}
