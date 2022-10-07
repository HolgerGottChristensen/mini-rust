use proc_macro2::Span;
use syn::parse::{Parse, ParseStream};
use syn::{braced, Error, parenthesized, parse, Token};
use syn::token::Paren;
use crate::kind::Kind::Star;

#[derive(Debug)]
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

impl Parse for Kind {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Paren) && input.peek2(Token!(=>)) {
            let content;
            let _ = parenthesized!(content in input);

            let kind: Kind = content.parse()?;
            let _: Token![=>] = input.parse()?;

            Ok(Kind::Arrow(Box::new(kind), Box::new(input.parse()?)))

        } else if input.peek(Paren) {
            let content;
            let _ = parenthesized!(content in input);

            let kind: Kind = content.parse()?;

            Ok(kind)

        } else if input.peek(Token!(*)) && input.peek2(Token!(=>)) {
            let _: Token![*] = input.parse()?;
            let _: Token![=>] = input.parse()?;

            Ok(Kind::Arrow(Box::new(Star), Box::new(input.parse()?)))
        } else if input.peek(Token!(*)) {
            let _: Token![*] = input.parse()?;
            Ok(Star)
        } else {
            Err(Error::new(Span::call_site(), "Could not match kind"))
        }
    }
}