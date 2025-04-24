use std::path::PathBuf;

#[derive(Debug, Clone, Default, PartialEq, Eq, clap::Parser)]
pub struct Args {
    pub path: PathBuf,
}
impl Args {
    pub fn parse() -> Self {
        clap::Parser::parse()
    }
}
