use crate::{error::MGLError, saving::token::TokenStream};

pub trait Parse: Sized {
    fn parse(stream: &mut TokenStream) -> Result<Self, MGLError>;
}
