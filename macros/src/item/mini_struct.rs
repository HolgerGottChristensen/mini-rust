use std::fmt::{Debug, Formatter};
use proc_macro2::Ident;
use syn::punctuated::Punctuated;
use syn::{braced, Field, Token, Type};
use syn::parse::{Parse, Parser, ParseStream};
use syn::token::{Brace, Colon, Comma, Struct};
use crate::{MiniIdent, MiniType};

#[derive(PartialEq, Clone)]
pub struct MiniStruct {
    pub struct_token: Struct,
    pub ident: MiniIdent,
    // pub generics: Generics,
    pub brace: Brace,
    pub fields: Punctuated<MiniStructField, Comma>
}

impl Debug for MiniStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MiniStruct")
            .field("ident", &self.ident)
            .field("fields", &self.fields.iter().collect::<Vec<_>>())
            .finish()
    }
}

impl Parse for MiniStruct {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;

        Ok(MiniStruct {
            struct_token: Struct::parse(input)?,
            ident: MiniIdent::parse(input)?,
            brace: braced!(content in input),
            fields: content.parse_terminated(MiniStructField::parse)?
        })
    }
}

#[derive(PartialEq, Clone)]
pub struct MiniStructField {
    pub ident: MiniIdent,
    pub colon_token: Colon,
    pub ty: Type,
}

impl Debug for MiniStructField {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MiniStructField")
            .field("ident", &self.ident)
            .field("ty", &MiniType(self.ty.clone()))
            .finish()
    }
}

impl Parse for MiniStructField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(MiniStructField {
            ident: MiniIdent::parse(input)?,
            colon_token: Colon::parse(input)?,
            ty: Type::parse(input)?,
        })
    }
}