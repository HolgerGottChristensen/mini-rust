use std::fmt::{Debug, Formatter};
use quote::ToTokens;
use syn::{ExprLit, Lit};
use syn::parse::{Parse, ParseStream};

#[derive(PartialEq, Clone, Debug)]
pub struct MiniLitExpr {
    pub lit: Lit,
}

impl Parse for MiniLitExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lit = Lit::parse(input)?;

        Ok(MiniLitExpr {
            lit
        })
    }
}