use std::fmt::{Debug, Formatter};
use paris::formatter::colorize_string;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::Type;
use crate::TYPE_COLOR;

#[derive(PartialEq, Clone)]
pub struct MiniType(pub Type);

impl Parse for MiniType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(MiniType(Type::parse(input)?))
    }
}

impl Debug for MiniType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = colorize_string(format!("{}{}</>", TYPE_COLOR, self.0.to_token_stream().to_string()));
        write!(f, "{}", string)
    }
}