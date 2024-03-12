mod ast;
mod cli;
mod lexer;
mod parser;
use anyhow::Result;
use ron::{self, ser::PrettyConfig};
use std::io::Read;

use crate::{cli::CliArgs, parser::Parser};

fn main() -> Result<()> {
    let args = CliArgs::parse();
    let mut file = std::fs::File::open(&args.input)?;
    let mut buffer = vec![];
    file.read_to_end(&mut buffer)?;
    let text = String::from_utf8(buffer)?;

    let parser = Parser::new(&text, args.input);

    let source_file = parser.parse_source_file();
    eprintln!(
        "{}",
        ron::ser::to_string_pretty(&source_file, PrettyConfig::new()).unwrap(),
    );

    panic!();
}
