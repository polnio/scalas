mod error;
mod parser;

use chumsky::Parser as _;
use parser::parser;

fn main() {
    let mut args = std::env::args();
    let exe = args.next().unwrap();
    let Some(path) = args.next() else {
        eprintln!("Usage: {} <file>", exe);
        std::process::exit(1);
    };
    let src = match std::fs::read_to_string(&path) {
        Ok(src) => src,
        Err(err) => {
            eprintln!("Error reading file {}: {}", path, err);
            std::process::exit(1);
        }
    };
    let result = match parser().parse(&src).into_result() {
        Ok(result) => result,
        Err(errs) => {
            for err in errs {
                error::report(path.clone(), src.clone(), err);
            }
            std::process::exit(1);
        }
    };
    dbg!(result);
}
