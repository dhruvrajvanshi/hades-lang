mod compiler;
mod core;
mod parser;
mod syntax;

use std::path::PathBuf;

use crate::core::IO;
use compiler::{Compiler, Flags};

fn main() -> IO<()> {
    let mut compiler = Compiler::new(Flags {
        sources: vec![PathBuf::from("test/hello_world.hds")],
        c_sources: vec![],
        module_directories: vec![PathBuf::from("test")],
        output_path: PathBuf::from("a.out"),
    });
    compiler.build()?;
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::compiler::{Compiler, Flags};
    use crate::core::IO;
    use std::{
        fs::{create_dir, read_dir, remove_dir_all},
        path::PathBuf,
        vec,
    };

    #[test]
    fn run_test_suite() -> IO<()> {
        println!("Running test suite...");
        let build_directory = PathBuf::from("test_build");
        remove_dir_all("test_build")?;
        create_dir("test_build")?;
        let test_directory = PathBuf::from("test");
        let runtime_file = PathBuf::from("runtime.c");
        for entry_r in read_dir("test")? {
            let entry = entry_r?;
            let path = entry.path();
            if !format!("{:?}", path.as_path()).contains("hello_world") {
                continue;
            }
            if path.is_dir() {
                continue;
            }
            let file_type = path
                .extension()
                .expect(format!("Found file with no extension: {:?}", path).as_str())
                .to_str()
                .expect("Unknown error");
            if file_type != "hds" {
                continue;
            }

            let output_file_name = path
                .file_stem()
                .expect("Invalid path")
                .to_str()
                .expect("Unknown error");

            println!("Running test for {:?}", path);

            let output_path = build_directory.as_path().join(output_file_name);

            let mut compiler = Compiler::new(Flags {
                sources: vec![path.clone()],
                module_directories: vec![test_directory.clone()],
                c_sources: vec![runtime_file.clone()],
                output_path: output_path.clone(),
            });

            compiler.build()?;

            assert!(
                output_path.exists(),
                format!("Expected {:?} to exist after Compiler::build", output_path)
            )
        }
        Ok(())
    }
}
