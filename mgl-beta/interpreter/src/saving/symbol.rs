use std::fmt;

use crate::loc::SourceLoc;

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: Box<str>,
    pub line: usize,
    pub col: usize,
}

impl Symbol {
    pub fn new(name: &str, line: usize, col: usize) -> Self {
        Self {
            name: name.into(),
            line,
            col,
        }
    }

    pub fn to_source_loc(&self) -> SourceLocation {
        SourceLoc {
            line: self.line,
            col: self.col,
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
