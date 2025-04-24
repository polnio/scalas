use std::path::PathBuf;

#[derive(Debug, Clone, Default, PartialEq, Eq, clap::Parser)]
pub struct Args {
    pub path: PathBuf,
    #[clap(short, long)]
    pub output: Option<PathBuf>,
    #[clap(long)]
    pub print_asm: bool,
}
impl Args {
    pub fn parse() -> Self {
        clap::Parser::parse()
    }
}
