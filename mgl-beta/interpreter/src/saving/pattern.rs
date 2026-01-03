use crate::{
    error::{ErrorType, MGLError}, interpret::Interpret, loc::SourceLoc, parse::Parse, saving::{
        literal::Literal, symbol::Symbol, token_type::TokenType, r#type::{TupleType, Type}
    }
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
                let (ident, r#type) = <(Pattern, Option<Type>)>::parse(stream)?;

                idents.push(ident);
                match r#type {
                    Some(val) => types.push(val),
                    None => (),
                }

                if !types.is_empty() {
                    let token = stream.peek();
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
        Ok((Pattern::Ident(ident), None))
    }
}

impl Interpret for (Pattern, Option<Type>, Literal) {
    fn interpret(&mut self, env: &mut crate::enviroment::GlobalEnv) -> Result<(), MGLError> {
        match &self.0 {
            Pattern::Ident(sym) => env.env.define(sym.clone(), self.2.clone(), self.1.clone()),
            Pattern::Tuple(tuple) => {
                if let Literal::Tuple(tuple_lit) = &self.2 {
                    if tuple.0.len() != tuple_lit.len() {
                        return Err(MGLError { error_type: ErrorType::PatternError, msg: String::from("pattern on the left hand side does not math expression on the right hand side"), loc: self.0.get_loc() })
                    }

                    match &self.1 {
                        Some(val) if &Literal::Tuple(tuple_lit.clone()).to_type() != val => return Err(MGLError { error_type: ErrorType::TypeError, msg: format!("values of type '{}' does not match type '{}'", Literal::Tuple(tuple_lit.clone()).display()?, val), loc: self.0.get_loc() }),
                        val => {
                            let mut iter = tuple_lit.iter();
                            for pat in tuple.0.clone().into_iter() {
                                (pat, val.clone(), iter.next().unwrap().clone()).interpret(env)?;
                            }
                        }
                    }
                    Ok(())
                } else {
                    return Err(MGLError { error_type: ErrorType::PatternError, msg: String::from("pattern on the left hand side does not match expression on the right hand side"), loc: self.0.get_loc() })
                }
            } 
        }
    }
}

impl Pattern {
    fn get_loc(&self) -> Option<SourceLoc> {
        match self {
            Pattern::Ident(sym) => Some(sym.into()),
            Pattern::Tuple(tuple_pat) => match tuple_pat.0.get(0) {
                Some(val) => return val.get_loc(),
                None => None
            }
        }
    }
}
