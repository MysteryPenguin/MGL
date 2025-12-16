use std::collections::HashMap;

use crate::{
    error::{ErrorType, MGLError},
    parse::Parse,
    saving::{
        expr::Expr, pattern::Pattern, stmt::Stmt, symbol::Symbol, token::TokenStream,
        token_type::TokenType, r#type::Type,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Struct(ObjectTemplItem),
    Class(ObjectTemplItem),
    Fn(FnItem),
    GlobalVar(GlobalVarItem),
}

impl Parse for Item {
    fn parse(stream: &mut TokenStream) -> Result<Self, MGLError> {
        if stream.match_tokens(&[TokenType::Struct]) {
            return Ok(Item::Struct(ObjectTemplItem::parse(stream)?));
        }

        if stream.match_tokens(&[TokenType::Class]) {
            return Ok(Item::Class(ObjectTemplItem::parse(stream)?));
        }

        if stream.match_tokens(&[TokenType::Fn]) {
            return Ok(Item::Fn(FnItem::parse(stream)?));
        }

        if stream.match_tokens(&[TokenType::Const]) {
            return Ok(Item::GlobalVar(GlobalVarItem::parse(stream)?));
        }

        let token = stream.peek();

        Err(MGLError {
            error_type: ErrorType::Pass,
            msg: String::from("error is passed"),
            loc: Some(token.into()),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectTemplItem {
    pub name: Symbol,
    pub fields: HashMap<Box<str>, Type>,
    pub public: bool,
}

impl Parse for ObjectTemplItem {
    fn parse(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let name = stream.consume(
            TokenType::Ident,
            String::from("expected identifier for class/struct"),
        )?;
        stream.consume(
            TokenType::LeftBrace,
            String::from("expected '{' after class/struct name"),
        )?;

        let mut fields: HashMap<_, _> = HashMap::new();

        while !stream.check(&TokenType::RightBrace) && !stream.is_at_end() {
            let field: Symbol = stream
                .consume(
                    TokenType::Ident,
                    String::from("expected identifier in class declaration"),
                )?
                .into();

            stream.consume(
                TokenType::Colon,
                String::from("expected ':' after field declaration"),
            )?;

            let r#type = Type::parse(stream)?;

            fields.insert(field, r#type);
        }

        Ok(ObjectTemplItem {
            name,
            fields,
            public: false,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnItem {
    pub name: Symbol,
    pub param: (Pattern, Type),
    pub body: Box<Stmt>,
    pub public: bool,
}

impl Parse for FnItem {
    fn parse(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let name = stream
            .consume(
                TokenType::Ident,
                String::from("expected ident in function declaration"),
            )?
            .into();

        let (pattern, r#type) = <(Pattern, Option<Type>)>::parse(stream)?;

        let token = stream.peek();

        let r#type = match (stream.match_tokens(&[TokenType::Colon]), r#type) {
            (true, Some(r#type)) => {
                return Err(MGLError {
                    error_type: ErrorType::PatternError,
                    msg: String::from(
                        "a type cannot be annotated if this already happened in the pattern",
                    ),
                    loc: Some(token.into()),
                });
            }
            (true, None) => Type::parse(stream)?,
            (false, Some(r#type)) => r#type,
            (false, None) => {
                return Err(MGLError {
                    error_type: ErrorType::TypeError,
                    msg: String::from("type annotation required in function parameters"),
                    loc: Some(token.into()),
                });
            }
        };

        let body = Box::new(Stmt::parse(stream)?);

        Ok(FnItem {
            name,
            param: (pattern, r#type),
            body,
            public: false,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVarItem {
    pub pattern: Pattern,
    pub value: Expr,
    pub public: bool,
    pub r#type: Option<Type>,
}

impl Parse for GlobalVarItem {
    fn parse(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let (pattern, r#type) = <(Pattern, Option<Type>)>::parse(stream)?;

        let r#type = match (stream.match_tokens(&[TokenType::Colon]), r#type) {
            (true, Some(val)) => {
                return Err(MGLError {
                    error_type: ErrorType::PatternError,
                    msg: String::from(
                        "a type cannot be annotated if this already happened in the pattern",
                    ),
                    loc: Some(token.into()),
                });
            }
            (true, None) => Some(Type::parse(stream)?),
            (false, Some(val)) => Some(val),
            (false, None) => None,
        };

        let value = Expr::parse(stream)?;

        Ok(GlobalVarItem {
            pattern,
            value,
            public: false,
            r#type,
        })
    }
}
