use std::fmt;

use crate::{
    interpreter::Interpreter,
    saving::{symbol::Symbol, r#type::Type},
};

use super::{
    callable::NativeFn,
    error::Error,
    symbol::SourceLocation,
    r#type::{FnType, LiteralType},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Literal(Literal),
    Type(Type),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(lit) => write!(f, "{lit}"),
            Self::Type(r#type) => write!(f, "{}", r#type),
        }
    }
}

impl Value {
    pub fn to_type(&self, interpreter: &Interpreter) -> Type {
        match self {
            Self::Literal(lit) => Type::Literal(Some(lit.to_type(interpreter))),
            Self::Type(_) => Type::Type,
        }
    }

    pub fn to_string(&self) -> Option<String> {
        match self {
            Self::Literal(Literal::String(str)) => Some(str.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
    Bool(bool),
    Fn {
        name: Symbol,
        id: u64,
        instance: Option<Box<Value>>,
    },
    Imported(Imported),
    Closure(u64),
    NativeFn(NativeFn),
    Class {
        name: Symbol,
        id: u64,
    },
    Instance {
        name: Symbol,
        id: u64,
    },
    Void,
    None,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(str) => write!(f, "{str}"),
            Self::Number(num) => write!(f, "{num}"),
            Self::Bool(bool) => write!(f, "{bool}"),
            Self::NativeFn(function) => write!(f, "{function}"),
            Self::Fn {
                name,
                id: _,
                instance: _,
            } => write!(f, "<fn {name}>"),
            Self::Imported(Imported {
                sym,
                url: _
            }) => write!(f, "<template {sym}>"),
            Self::Class { name, id: _ } => write!(f, "<class {name}>"),
            Self::Instance { name, id: _ } => write!(f, "<instance {name}>"),
            Self::Void => write!(f, "void"),
            Self::None => write!(f, "none"),
            Self::Closure(id) => write!(f, "{id}"),
        }
    }
}

impl Literal {
    pub fn to_type(&self, interpreter: &Interpreter) -> LiteralType {
        match self {
            Literal::String(_) => LiteralType::String,
            Literal::Number(_) => LiteralType::Number,
            Literal::Bool(_) => LiteralType::Bool,
            Literal::Fn {
                name: _,
                id,
                instance: _,
            } => {
                let function = interpreter.get_function(*id);
                let params = function.params.iter().map(|p| p.1.clone()).collect();
                LiteralType::Fn(FnType {
                    params,
                    return_type: Box::new(Type::Literal(Some(LiteralType::Void))),
                })
            },
            Literal::Imported(Imported {
                sym: _,
                url: _
            }) => LiteralType::None,
            Literal::NativeFn(_) => LiteralType::NativeFn,
            Literal::Class { name: _, id: _ } => LiteralType::Class,
            Literal::Instance { name, id: _ } => LiteralType::Instance(name.name.clone()),
            Literal::None => LiteralType::None,
            Literal::Void => LiteralType::Void,
            Literal::Closure(id) => {
                let closure = interpreter.get_closure(*id);
                let params = closure.params.iter().map(|p| p.1.clone()).collect();
                LiteralType::Fn(FnType {
                    params,
                    return_type: Box::new(Type::Literal(Some(LiteralType::Void))),
                })
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: Symbol,
    pub r#type: Type,
    pub value: Box<Value>,
    pub is_pub: bool
}

impl Identifier {
    pub fn new(interpreter: &Interpreter, name: Symbol, value: Value, is_pub: bool) -> Self {
        Self {
            name,
            r#type: value.to_type(interpreter),
            value: Box::new(value),
            is_pub
        }
    }

    pub fn with_type(
        interpreter: &Interpreter,
        name: Symbol,
        r#type: Type,
        value: Value,
        is_pub: bool
    ) -> Result<Self, Error> {
        if value.to_type(interpreter) != r#type {
            return Err(interpreter.error_builder.build(
                super::error::ErrorType::Type,
                format!(
                    "expect value of type '{}'. Found '{}'",
                    r#type,
                    value.to_type(interpreter)
                ),
                SourceLocation {
                    line: name.line,
                    col: name.col,
                },
            ));
        }
        Ok(Self {
            name,
            r#type,
            value: Box::new(value),
            is_pub
        })
    }

    pub fn native_fn(native_fn: NativeFn) -> Self {
        Self {
            name: Symbol::new(native_fn.name.clone(), 1337, 1337),
            r#type: Type::Literal(Some(LiteralType::NativeFn)),
            value: Box::new(Value::Literal(Literal::NativeFn(native_fn))),
            is_pub: true
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Imported {
    pub sym: Symbol,
    pub url: String
}
