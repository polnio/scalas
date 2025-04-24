mod args;
mod error;
mod parser;

use args::Args;
use chumsky::Parser as _;
use parser::parser;

fn main() {
    let args = Args::parse();
    let src = match std::fs::read_to_string(&args.path) {
        Ok(src) => src,
        Err(err) => {
            eprintln!("Error reading file {}: {}", args.path.display(), err);
            std::process::exit(1);
        }
    };
    let result = match parser().parse(&src).into_result() {
        Ok(result) => result,
        Err(errs) => {
            let path_str = args.path.to_string_lossy().into_owned();
            for err in errs {
                error::report(path_str.clone(), src.clone(), err);
            }
            std::process::exit(1);
        }
    };
    dbg!(result);
}
