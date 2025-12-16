use crate::{
    error::{ErrorType, MGLError},
    parse::Parse,
    saving::{expr::Expr, item::Item, token::TokenStream},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Item(Item),
    Expr(Expr),
}

impl Parse for Stmt {
    fn parse(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let item = Item::parse(stream);

        match item {
            Ok(item) => Ok(Stmt::Item(item)),
            Err(err) if err.error_type == ErrorType::Pass => Ok(Stmt::Expr(Expr::parse(stream)?)),
            Err(err) => Err(err),
        }
    }
}
