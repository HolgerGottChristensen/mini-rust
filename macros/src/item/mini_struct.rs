use proc_macro2::Ident;
use syn::punctuated::Punctuated;
use syn::{braced, Field, Token, Type};
use syn::parse::{Parse, Parser, ParseStream};
use syn::token::{Brace, Colon, Comma, Struct};

#[derive(PartialEq, Clone, Debug)]
pub struct MiniStruct {
    pub struct_token: Struct,
    pub ident: Ident,
    // pub generics: Generics,
    pub brace: Brace,
    pub fields: Punctuated<MiniStructField, Comma>
}

impl Parse for MiniStruct {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;

        Ok(MiniStruct {
            struct_token: Struct::parse(input)?,
            ident: Ident::parse(input)?,
            brace: braced!(content in input),
            fields: content.parse_terminated(MiniStructField::parse)?
        })
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct MiniStructField {
    pub ident: Ident,
    pub colon_token: Colon,
    pub ty: Type,
}

impl Parse for MiniStructField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(MiniStructField {
            ident: Ident::parse(input)?,
            colon_token: Colon::parse(input)?,
            ty: Type::parse(input)?,
        })
    }
}