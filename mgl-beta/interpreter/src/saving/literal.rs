use std::collections::HashMap;

use crate::{
    error::{ErrorType, MGLError},
    saving::r#type::Type,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    UInt(usize),
    Int(isize),
    U128(u128),
    I128(i128),
    Float(f64),
    Bool(bool),
    Tuple(Vec<Literal>),
    Ref(usize),
    Fn(usize),
    Object(usize),
    Struct(ObjectTempl),
    StructInst(StructInst),
    Class(ObjectTempl),
    ClassInst(usize),
    List(usize),
    Ident(Box<str>),
    Type(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectTempl {
    pub name: Box<str>,
    pub fields: HashMap<String, Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInst {
    pub name: Box<str>,
    pub fields: HashMap<String, Literal>,
}

impl TryFrom<&Literal> for String {
    type Error = MGLError;
    fn try_from(value: &Literal) -> Result<Self, Self::Error> {
        value.display()
    }
}

impl Literal {
    fn display(&self) -> Result<String, MGLError> {
        match self {
            Literal::String(string) => Ok(string.to_string()),
            Literal::UInt(uint) => Ok(uint.to_string()),
            Literal::Int(int) => Ok(int.to_string()),
            Literal::U128(u128) => Ok(u128.to_string()),
            Literal::I128(i128) => Ok(i128.to_string()),
            Literal::Bool(bool) => Ok(bool.to_string()),
            Literal::Float(float) => Ok(float.to_string()),
            Literal::Tuple(elements) => {
                let elements = elements
                    .iter()
                    .map(|l| l.display())
                    .collect::<Result<Vec<String>, MGLError>>()?
                    .join(", ");

                Ok(format!("({elements})"))
            }
            Literal::Ref(id) => Ok(format!("[{:#x}]", *id)),
            Literal::Fn(id) => Ok(format!("[fn {:#x}]", *id)),
            Literal::Object(id) => Ok(format!("[object {:#x}]", *id)),
            Literal::List(id) => Ok(format!("[list {:#x}]", *id)),
            Literal::Struct(obj) => Ok(format!("[struct {}]", obj.name)),
            Literal::StructInst(obj) => Err(MGLError {
                error_type: ErrorType::ImplementationError,
                msg: format!(
                    "Instance of struct '{}' cannot be printed because the struct does not implement the Display trait.",
                    obj.name
                ),
                loc: None,
            }),
            Literal::Class(obj) => Ok(format!("[class {}]", obj.name)),
            Literal::ClassInst(id) => Ok(format!("[classinst {:#x}]", *id)),
            Literal::Ident(ident) => Ok(format!("{}", *ident)),
            Literal::Type(ty) => Ok(format!("{ty}")),
        }
    }
}
