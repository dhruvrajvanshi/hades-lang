use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    #[arg(short, long)]
    pub input: PathBuf,
    #[arg(short, long)]
    pub output: Option<String>,
}
impl CliArgs {
    // To avoid having to import Parser trait by name, which conflicts
    // with the syntax parser, we re-export the parse method here.
    pub fn parse() -> Self {
        Parser::parse()
    }
}
