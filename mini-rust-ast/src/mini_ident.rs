use std::fmt::{Debug, Formatter};
use paris::formatter::colorize_string;
use proc_macro2::Ident;
use syn::parse::{Parse, ParseStream};
use crate::IDENT_COLOR;

#[derive(PartialEq, Clone)]
pub struct MiniIdent(pub Ident);

impl ToString for MiniIdent {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl Parse for MiniIdent {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(MiniIdent(Ident::parse(input)?))
    }
}

impl Debug for MiniIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = colorize_string(format!("{}{}</>", IDENT_COLOR, self.0.to_string()));
        write!(f, "{}", string)
    }
}