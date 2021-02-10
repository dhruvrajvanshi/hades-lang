use crate::core::IO;

pub struct Compiler {

}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {}
    }

    pub fn build(&mut self) -> IO<()> {
        Ok(())
    }
}
