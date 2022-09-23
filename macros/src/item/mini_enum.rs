use proc_macro2::Ident;
use syn::punctuated::Punctuated;
use syn::{braced, Field, parenthesized, Token, Type};
use syn::parse::{Parse, Parser, ParseStream};
use syn::token::{Brace, Colon, Comma, Enum, Paren, Struct};

#[derive(PartialEq, Clone, Debug)]
pub struct MiniEnum {
    pub enum_token: Enum,
    pub ident: Ident,
    // pub generics: Generics,
    pub brace: Brace,
    pub fields: Punctuated<MiniEnumVariant, Comma>
}

impl Parse for MiniEnum {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;

        Ok(MiniEnum {
            enum_token: Enum::parse(input)?,
            ident: Ident::parse(input)?,
            brace: braced!(content in input),
            fields: content.parse_terminated(MiniEnumVariant::parse)?
        })
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct MiniEnumVariant {
    pub ident: Ident,
    pub paren: Option<Paren>,
    pub items: Option<Punctuated<Type, Comma>>,
}

impl Parse for MiniEnumVariant {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = Ident::parse(input)?;

        let (paren, items) = if input.peek(Paren) {
            let content;
            let paren = parenthesized!(content in input);
            let punctuated = content.parse_terminated(Type::parse)?;

            (Some(paren), Some(punctuated))
        } else {
            (None, None)
        };

        Ok(MiniEnumVariant {
            ident,
            paren,
            items,
        })
    }
}