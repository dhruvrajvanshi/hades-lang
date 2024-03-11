mod lexer;
use anyhow::Result;
use std::{io::Read, path::PathBuf};

use crate::lexer::Lexer;

fn main() -> Result<()> {
    let path = PathBuf::from("./test/empty_main.hds");
    let mut file = std::fs::File::open(&path)?;
    let mut buffer = vec![];
    file.read_to_end(&mut buffer)?;
    let text = String::from_utf8(buffer)?;

    let mut lexer = Lexer::new(&text, path);

    lexer.next_token();

    panic!();
}
