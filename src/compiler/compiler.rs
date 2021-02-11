use super::flags::Flags;
use crate::core::{SourcePath, IO};
use crate::parser::Parser;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::PathBuf;

pub struct Compiler {
    pub flags: Flags,
    next_source_path: RefCell<usize>,
    source_paths: RefCell<HashMap<PathBuf, SourcePath>>,
}

impl Compiler {
    pub fn new(flags: Flags) -> Compiler {
        Compiler {
            flags,
            source_paths: RefCell::new(HashMap::new()),
            next_source_path: RefCell::new(0),
        }
    }

    pub fn build(&mut self) -> IO<()> {
        for input_path in &self.flags.sources {
            let input = read_to_string(input_path)?;
            let mut parser = Parser::new(input.as_str(), self.make_source_path(input_path));
            let source_file = parser.parse_source_file();
        }
        todo!()
    }

    pub fn make_source_path(&self, path: &PathBuf) -> SourcePath {
        let canonicalized = path.canonicalize().expect("Unable to canonicalize path");
        let mut source_paths = self.source_paths.borrow_mut();
        match source_paths.get(canonicalized.as_path()) {
            Some(path) => *path,
            None => {
                let mut id_ref = self.next_source_path.borrow_mut();
                let id = *id_ref;
                *id_ref = id + 1;
                let result = SourcePath(id);
                source_paths.insert(canonicalized, result);
                result
            }
        }
    }
}
