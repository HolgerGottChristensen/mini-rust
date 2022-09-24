use std::fmt::{Debug, Formatter};
use paris::formatter::colorize_string;
use quote::ToTokens;
use syn::{ExprLit, Lit};
use syn::parse::{Parse, ParseStream};

#[derive(PartialEq, Clone)]
pub struct MiniLitExpr {
    pub lit: Lit,
}

impl Debug for MiniLitExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = colorize_string(format!("<green>{}</>", self.lit.to_token_stream().to_string()));
        write!(f, "{}", s)
    }
}

impl Parse for MiniLitExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lit = Lit::parse(input)?;

        Ok(MiniLitExpr {
            lit
        })
    }
}