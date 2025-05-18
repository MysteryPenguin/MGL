use std::fmt;

use crate::File;

use super::symbol::SourceLocation;

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorBuilder(pub File);

impl ErrorBuilder {
    pub fn build(&self, error_type: ErrorType, message: String, loc: SourceLocation) -> Error {
        Error {
            error_type,
            message,
            loc,
            file: self.0.clone()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub error_type: ErrorType,
    pub message: String,
    pub loc: SourceLocation,
    pub file: File
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let content = &mut self
            .file
            .source
            .split('\n')
            .map(String::from)
            .collect::<Vec<String>>()[self.loc.line - 1];

        content.insert_str(if self.loc.col as i16 - 1 >= 0 { self.loc.col - 1 } else { 0 }, "\x1b[1;31m");
        content.insert_str(self.loc.col + 7, "\x1b[0m");

        let line_space = " ".repeat(self.loc.line.to_string().len());
        let mut mark_line = " ".repeat(if self.loc.col as i16 - 1 >= 0 { self.loc.col - 1 } else { 0 });
        mark_line.push_str("\x1b[1;31m^\x1b[0m");

        let to_write = format!(
            "\x1b[1;31m{} error\x1b[1;39m: {}\x1b[0m
  \x1b[1;96m-->\x1b[0m {}:{}:{}
{line_space} |
{} | {content}
{line_space} | {mark_line}
",
            &self.error_type,
            &self.message,
            &self.file.path.join("/"),
            self.loc.line,
            self.loc.col,
            self.loc.line
        );
        write!(f, "{to_write}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorType {
    Syntax,
    Runtime,
    Overflow,
    Type
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Syntax => write!(f, "syntax"),
            Self::Runtime => write!(f, "runtime"),
            Self::Overflow => write!(f, "overflow"),
            Self::Type => write!(f, "type")
        }
    }
}
