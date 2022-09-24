use std::fmt::{Debug, Formatter};
use paris::formatter::colorize_string;
use proc_macro2::Ident;
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::{braced, Field, parenthesized, Token, Type};
use syn::parse::{Parse, Parser, ParseStream};
use syn::token::{Brace, Colon, Comma, Enum, Paren, Struct};
use crate::{MiniIdent, MiniType};

#[derive(PartialEq, Clone)]
pub struct MiniEnum {
    pub enum_token: Enum,
    pub ident: MiniIdent,
    // pub generics: Generics,
    pub brace: Brace,
    pub fields: Punctuated<MiniEnumVariant, Comma>
}

impl Debug for MiniEnum {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(stringify!(MiniEnum))
            .field("ident", &self.ident)
            .field("fields", &self.fields.iter().collect::<Vec<_>>())
            .finish()
    }
}

impl Parse for MiniEnum {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;

        Ok(MiniEnum {
            enum_token: Enum::parse(input)?,
            ident: MiniIdent::parse(input)?,
            brace: braced!(content in input),
            fields: content.parse_terminated(MiniEnumVariant::parse)?
        })
    }
}

#[derive(PartialEq, Clone)]
pub struct MiniEnumVariant {
    pub ident: MiniIdent,
    pub paren: Option<Paren>,
    pub items: Option<Punctuated<Type, Comma>>,
}

impl Debug for MiniEnumVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct(stringify!(MiniEnumVariant));

        s.field("ident", &self.ident);

        if let Some(items) = &self.items {
            s.field("items", &items.iter().map(|a| MiniType(a.clone())).collect::<Vec<_>>());
        }

        s.finish()
    }
}

impl Parse for MiniEnumVariant {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = MiniIdent::parse(input)?;

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