mod core;
mod compiler;

use compiler::Compiler;
use crate::core::IO;

fn main() -> IO<()> {
    let mut compiler = Compiler::new();
    compiler.build()?;
    Ok(())
}


#[cfg(test)]
mod test {
    use std::fs::{read_dir};
    use crate::core::IO;
    #[test]
    fn run_test_suite() -> IO<()> {
        println!("Running test suite...");
        for entry_r in read_dir("test")? {
            let entry = entry_r?;
            let path = entry.path();
            if path.is_dir() {
                continue;
            }
            let file_type = path.extension()
                .expect(format!("Found file with no extension: {:?}", path).as_str())
                .to_str()
                .expect("Unknown error");
            if file_type != "hds" {
                continue;
            }

            println!("Running test for {:?}", path);

        }
        Ok(())
    }
}
