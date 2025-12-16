use crate::{
    error::{ErrorType, MGLError},
    parse::Parse,
    saving::{
        token_type::TokenType,
        r#type::{TupleType, Type},
    },
};

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(Symbol),
    Tuple(TuplePat),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TuplePat(Vec<Pattern>);

impl Parse for (Pattern, Option<Type>) {
    fn parse(stream: &mut super::token::TokenStream) -> Result<Self, crate::error::MGLError> {
        if stream.match_tokens(&[TokenType::LeftParen]) {
            let mut types = Vec::new();
            let mut idents = Vec::new();
            while !stream.check(&TokenType::RightParen) && !stream.is_at_end() {
                let token = stream.peek();
                let (ident, r#type) = <(Pattern, Option<Type>)>::parse(stream)?;

                idents.push(ident);
                match r#type {
                    Some(val) => types.push(val),
                    None => (),
                }

                if !types.is_empty() {
                    return Err(MGLError {
                        error_type: ErrorType::PatternError,
                        msg: String::from(
                            "types have to follow after the identifiers if at least one element of the tuple has a type annotation inside the pattern",
                        ),
                        loc: Some(token.into()),
                    });
                }

                stream.consume(
                    TokenType::Comma,
                    String::from("expected ',' after pattern value"),
                )?;
            }

            stream.consume(
                TokenType::RightParen,
                String::from("expect ')' after tuple pattern"),
            )?;

            return Ok((
                Pattern::Tuple(TuplePat(idents)),
                if types.is_empty() {
                    Some(Type::Tuple(Some(TupleType(types))))
                } else {
                    None
                },
            ));
        }

        let ident = stream
            .consume(TokenType::Ident, String::from("expected pattern"))?
            .into();
        Ok((ident, None))
    }
}
