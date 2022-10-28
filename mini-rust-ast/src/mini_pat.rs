use std::fmt::{Debug, Formatter};

use paris::formatter::colorize_string;
use quote::ToTokens;
use syn::{Pat, PatOr, Token};
use syn::parse::ParseStream;
use syn::punctuated::Punctuated;

use crate::PAT_COLOR;

pub struct MiniPat(pub Pat);

impl Debug for MiniPat {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = colorize_string(format!("{}{}</>", PAT_COLOR, self.0.to_token_stream().to_string()));
        write!(f, "{}", string)
    }
}

pub fn multi_pat(input: ParseStream) -> syn::Result<Pat> {
    multi_pat_impl(input, None)
}

pub fn multi_pat_with_leading_vert(input: ParseStream) -> syn::Result<Pat> {
    let leading_vert: Option<Token![|]> = input.parse()?;
    multi_pat_impl(input, leading_vert)
}

fn multi_pat_impl(input: ParseStream, leading_vert: Option<Token![|]>) -> syn::Result<Pat> {
    let mut pat: Pat = input.parse()?;
    if leading_vert.is_some()
        || input.peek(Token![|]) && !input.peek(Token![||]) && !input.peek(Token![|=])
    {
        let mut cases = Punctuated::new();
        cases.push_value(pat);
        while input.peek(Token![|]) && !input.peek(Token![||]) && !input.peek(Token![|=]) {
            let punct = input.parse()?;
            cases.push_punct(punct);
            let pat: Pat = input.parse()?;
            cases.push_value(pat);
        }
        pat = Pat::Or(PatOr {
            attrs: Vec::new(),
            leading_vert,
            cases,
        });
    }
    Ok(pat)
}