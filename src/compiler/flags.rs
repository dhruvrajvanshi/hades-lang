use std::path::PathBuf;

pub struct Flags {
    pub sources: Vec<PathBuf>,
    pub module_directories: Vec<PathBuf>,
    pub c_sources: Vec<PathBuf>,
    pub output_path: PathBuf,
}

impl Flags {
    pub fn from_args() -> Option<Flags> {
        None
    }
}