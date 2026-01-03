use std::collections::HashMap;

use crate::{
    error::{ErrorType, MGLError},
    saving::r#type::{RefType, TupleType, Type},
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
    ClassInst {
        name: Box<str>,
        reference: usize
    },
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
    pub fn display(&self) -> Result<String, MGLError> {
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
            Literal::ClassInst { name, reference: id }=> Ok(format!("[classinst {name} <{:#x}>]", *id)),
            Literal::Ident(ident) => Ok(format!("{}", *ident)),
            Literal::Type(ty) => Ok(format!("{ty}")),
        }
    }

    pub fn to_type(&self) -> Type {
        match self {
            &Literal::String(_) => Type::String,
            &Literal::Bool(_) => Type::Bool,
            &Literal::Int(_) => Type::Int,
            &Literal::UInt(_) => Type::UInt,
            &Literal::I128(_) => Type::U128,
            &Literal::U128(_) => Type::U128,
            &Literal::Float(_) => Type::Float,
            &Literal::Class(_) => Type::Class,
            &Literal::Struct(_) => Type::Struct,
            &Literal::ClassInst { ref name, .. } => Type::ObjectTemplInstance(name.clone()),
            &Literal::StructInst(ref obj) => Type::ObjectTemplInstance(obj.name.clone()),
            &Literal::Fn(_) => Type::Fn(None),
            &Literal::Object(_) => Type::Object(None),
            &Literal::Ref(_) => Type::Ref(None),
            &Literal::Tuple(ref tuple) => {
                let mut types = Vec::new();

                for lit in tuple {
                    types.push(lit.to_type());
                }
                Type::Tuple(Some(TupleType(types)))
            },
            &Literal::Ident(_) => Type::Ident,
            &Literal::Type(_) => Type::Type,
            &Literal::List(_) => Type::List(None)
        }
    }
}
