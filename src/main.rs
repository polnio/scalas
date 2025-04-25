mod args;
mod builtins;
mod compiler;
mod error;
mod parser;
mod tokenizer;

use args::Args;
use chumsky::input::Input as _;
use compiler::compile;
use parser::parse;
use tokenizer::tokenize;

fn main() {
    let args = Args::parse();
    let src = match std::fs::read_to_string(&args.path) {
        Ok(src) => src,
        Err(err) => {
            eprintln!("Error reading file {}: {}", args.path.display(), err);
            std::process::exit(1);
        }
    };
    let tokens = match tokenize(&src) {
        Ok(tokens) => tokens,
        Err(errs) => {
            let path_str = args.path.to_string_lossy().into_owned();
            for err in errs {
                error::report(path_str.clone(), src.clone(), err);
            }
            std::process::exit(1);
        }
    };
    let tokens = tokens
        .as_slice()
        .map((src.len()..src.len()).into(), |(t, s)| (t, s));
    let program = match parse(tokens) {
        Ok(program) => program,
        Err(errs) => {
            let path_str = args.path.to_string_lossy().into_owned();
            for err in errs {
                error::report(path_str.clone(), src.clone(), err);
            }
            std::process::exit(1);
        }
    };
    if let Err(err) = compile(program, &args) {
        eprintln!("{}", err);
    }
}
