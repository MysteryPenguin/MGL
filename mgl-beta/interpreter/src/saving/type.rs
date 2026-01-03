use std::{collections::HashMap, fmt::Display};

use crate::{
    error::{ErrorType, MGLError},
    parse::Parse,
    saving::{token::TokenStream, token_type::TokenType},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    String,
    UInt,
    Int,
    U128,
    I128,
    Char,
    Float,
    Bool,
    Struct,
    ObjectTemplInstance(Box<str>),
    Class,
    Fn(Option<FnType>),
    Object(Option<ObjectType>),
    List(Option<Box<Type>>),
    Tuple(Option<TupleType>),
    Or(Box<Type>, Box<Type>),
    Expr(Box<Type>),
    Range(Box<Type>, Box<Type>),
    Ref(Option<RefType>),
    Type,
    Never,
    Ident
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::String => write!(f, "string"),
            Type::UInt => write!(f, "uint"),
            Type::Int => write!(f, "int"),
            Type::U128 => write!(f, "u128"),
            Type::I128 => write!(f, "i128"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Ident => write!(f, "ident"),
            Type::Fn(function) => {
                if let Some(function) = function {
                    write!(f, "{function}")
                } else {
                    write!(f, "function")
                }
            }
            Type::Object(obj) => {
                if let Some(obj) = obj {
                    write!(f, "{obj}")
                } else {
                    write!(f, "object")
                }
            }
            Type::List(list) => {
                if let Some(list) = list {
                    write!(f, "[{list}]")
                } else {
                    write!(f, "list")
                }
            }
            Type::Tuple(tuple) => {
                if let Some(tuple) = tuple {
                    write!(f, "{}", tuple)
                } else {
                    write!(f, "tuple")
                }
            }
            Type::Or(left, right) => write!(f, "{left} | {right}"),
            Type::Expr(r#type) => write!(f, "({})", r#type),
            Type::Range(min, max) => write!(f, "{min}..{max}"),
            Type::Ref(reference) => match reference {
                Some(val) => write!(f, "{val}"),
                None => write!(f, "ref"),
            },
            Type::Type => write!(f, "type"),
            Type::Never => write!(f, "!"),
            Type::Struct => write!(f, "struct"),
            Type::ObjectTemplInstance(name) => write!(f, "{name}"),
            Type::Class => write!(f, "class"),
        }
    }
}

impl Parse for Type {
    fn parse(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let input_type = Type::template_types(stream)?;

        let output_type = if stream.match_tokens(&[TokenType::Arrow]) {
            Some(Type::template_types(stream)?)
        } else {
            None
        };

        match output_type {
            Some(r#type) => Ok(Type::Fn(Some(FnType { param: Box::new(input_type), return_type: Box::new(r#type) }))),
            None => Ok(input_type)
        }
    }
}

impl Type {
    fn single_eval(stream: &mut TokenStream) -> Result<Type, MGLError> {
        if stream.check(&TokenType::Ident) {
            let r#type = stream.consume(
                TokenType::Ident,
                String::from("expect type after type annotation"),
            )?;

            let r#type = match &*r#type.lexeme {
                "string" => Type::String,
                "uint" => Type::UInt,
                "int" => Type::Int,
                "u128" => Type::U128,
                "i128" => Type::I128,
                "float" => Type::Float,
                "char" => Type::Char,
                "fn" => Type::Fn(None),
                "object" => Type::Object(None),
                "list" => Type::List(None),
                "tuple" => Type::Tuple(None),
                "type" => Type::Type,
                "!" => Type::Never,
                "struct" => Type::Struct,
                "class" => Type::Class,
                "ident" => Type::Ident,
                obj_templ_name => Type::ObjectTemplInstance(obj_templ_name.into()),
            };

            return Ok(r#type);
        }

        let token = stream.peek();
        Err(MGLError {
            error_type: ErrorType::TypeError,
            msg: String::from("unknown type"),
            loc: Some(token.into()),
        })
    }

    fn template_types(stream: &mut TokenStream) -> Result<Type, MGLError> {
        if stream.match_tokens(&[TokenType::LeftBracket]) {
            let r#type = Self::template_types(stream)?;
            stream.consume(
                TokenType::RightBracket,
                String::from("expected ']' after list type definition"),
            )?;

            return Ok(Type::List(Some(Box::new(r#type))));
        }

        if stream.match_tokens(&[TokenType::LeftParen]) {
            let mut types = Vec::new();

            while !stream.check(&TokenType::LeftParen) && !stream.is_at_end() {
                let r#type = Self::template_types(stream)?;
                types.push(r#type);
                stream.consume(
                    TokenType::Comma,
                    String::from("expected ',' after type in tuple"),
                )?;
            }
            stream.consume(
                TokenType::RightParen,
                String::from("expect ')' after tuple type definition"),
            )?;

            return Ok(Self::Tuple(Some(TupleType(types))));
        }

        let r#type = Self::single_eval(stream)?;
        Ok(r#type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RefType {
    addr: Option<usize>,
    ref_type: Box<Type>,
}

impl Display for RefType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let address = if let Some(addr) = self.addr {
            format!("[{}]", addr)
        } else {
            String::from("&")
        };

        write!(f, "{}{}", address, *self.ref_type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnType {
    param: Box<Type>,
    return_type: Box<Type>,
}

impl Display for FnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.param, *self.return_type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectType {
    values: HashMap<String, Type>,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let values = self
            .values
            .iter()
            .map(|(key, value)| format!("{key:?}: {value}"))
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "{{ {values} }}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleType(pub Vec<Type>);

impl Display for TupleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({})",
            self.0
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}
