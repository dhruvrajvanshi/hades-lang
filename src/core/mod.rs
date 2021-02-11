pub type IO<T> = Result<T, Box<dyn std::error::Error>>;

/// A proxy for a single normalized source file path.
/// Compiler is responsible for creating these and
/// converting them back to &Path refs.
#[derive(Copy, Clone, Debug)]
pub struct SourcePath(pub usize);

#[derive(Copy, Clone, Debug)]
pub struct SourcePosition {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct SourceLocation {
    pub source_path: SourcePath,
    pub start: SourcePosition,
    pub stop: SourcePosition,
}

impl SourceLocation {
    pub fn between<Start: HasLocation, Stop: HasLocation>(
        start: &Start,
        stop: &Stop,
    ) -> SourceLocation {
        SourceLocation {
            source_path: start.location().source_path,
            start: start.location().start,
            stop: stop.location().stop,
        }
    }

    pub fn between_optional<Start: HasLocation, Stop: HasLocation, Opt: HasLocation>(
        start: &Start,
        stop: &Stop,
        option: &Option<Opt>,
    ) -> SourceLocation {
        match option {
            Some(t) => Self::between(start, t),
            None => Self::between(start, stop),
        }
    }
}

impl HasLocation for SourceLocation {
    fn location(&self) -> SourceLocation {
        *self
    }
}

/// A handle to an interned string.
/// Compiler creates and unwraps these
/// from/to strings
pub struct Name(pub u32);

pub trait HasLocation {
    fn location(&self) -> SourceLocation;
}

impl<T> HasLocation for Box<T>
where
    T: HasLocation,
{
    fn location(&self) -> SourceLocation {
        self.as_ref().location()
    }
}
