mod ast;
mod lexer;
mod parser;
use anyhow::Result;
use std::{io::Read, path::PathBuf};

use crate::parser::Parser;

fn main() -> Result<()> {
    let path = PathBuf::from("./test/empty_main.hds");
    let mut file = std::fs::File::open(&path)?;
    let mut buffer = vec![];
    file.read_to_end(&mut buffer)?;
    let text = String::from_utf8(buffer)?;

    let parser = Parser::new(&text, path);

    eprintln!("{:?}", parser.parse_source_file());

    panic!();
}
