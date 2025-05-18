use std::fmt;

use super::r#type::{LiteralType, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub line: usize,
    pub col: usize,
}

impl Symbol {
    pub fn new(name: String, line: usize, col: usize) -> Self {
        Self {
            name,
            line,
            col
        }
    }

    pub fn to_type(&self) -> Type {
        match self.name.as_str() {
            "literal" => Type::Literal(None),
            "type" => Type::Type,
            "string" => Type::Literal(Some(LiteralType::String)),
            "number" => Type::Literal(Some(LiteralType::Number)),
            "bool" => Type::Literal(Some(LiteralType::Bool)),
            "void" => Type::Literal(Some(LiteralType::Void)),
            "none" => Type::Literal(Some(LiteralType::None)),
            "class" => Type::Literal(Some(LiteralType::Class)),
            _ => Type::Literal(Some(LiteralType::Instance(self.name.clone())))
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub line: usize,
    pub col: usize
}