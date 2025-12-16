use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct SourceLoc {
    pub line: usize,
    pub col: usize,
}

impl Display for SourceLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}
