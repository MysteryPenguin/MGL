use std::fmt::Display;

use crate::loc::SourceLoc;

pub struct MGLError {
    pub error_type: ErrorType,
    pub msg: String,
    pub loc: Option<SourceLoc>,
}

impl MGLError {
    pub fn new(error_type: ErrorType, msg: String, loc: Option<SourceLoc>) -> Self {
        Self {
            error_type,
            msg,
            loc,
        }
    }
}

impl Display for MGLError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let location = if let Some(loc) = &self.loc {
            format!("Location: {loc}")
        } else {
            String::new()
        };

        write!(
            f,
            "\x1b[1;31m{}: \x1b[0;31m{}\n{location}",
            self.error_type, self.msg
        )
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ErrorType {
    SyntaxError,
    ImplementationError,
    OverflowException,
    StructureError,
    PatternError,
    TypeError,
    UndefinedVariableError,
    Pass,
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorType::SyntaxError => write!(f, "SyntaxError"),
            ErrorType::ImplementationError => write!(f, "ImplementationError"),
            ErrorType::OverflowException => write!(f, "OverflowException"),
            ErrorType::StructureError => write!(f, "StructureError"),
            ErrorType::PatternError => write!(f, "PatternError"),
            ErrorType::TypeError => write!(f, "TypeError"),
            ErrorType::UndefinedVariableError => write!(f, "UndefinedVariableError"),
            ErrorType::Pass => panic!("Case Pass without handling"),
        }
    }
}
