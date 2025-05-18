use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Literal(Option<LiteralType>),
    Type,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(lit_type) => {
                match lit_type {
                    Some(val) => write!(f, "{val}"),
                    None => write!(f, "lit")
                }
            },
            Self::Type => write!(f, "type"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    String,
    Number,
    Bool,
    Fn(FnType),
    Class,
    Instance(String),
    None,
    Void,
    NativeFn
}

impl fmt::Display for LiteralType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String => write!(f, "string"),
            Self::Number => write!(f, "number"),
            Self::Bool => write!(f, "bool"),
            Self::Fn(FnType { params, return_type }) => {
                let mut displayed_params = String::new();
                for param in params {
                    displayed_params.push_str(&(format!("{param}") + ", "));
                }

                displayed_params.pop();
                displayed_params.pop();

                write!(f, "({displayed_params}) -> {return_type}")
            },
            Self::Class => write!(f, "class"),
            Self::Instance(sym) => write!(f, "{sym}"),
            Self::None => write!(f, "none"),
            Self::Void => write!(f, "void"),
            Self::NativeFn => write!(f, "<native fn>")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnType {
    pub params: Vec<Type>,
    pub return_type: Box<Type>
}