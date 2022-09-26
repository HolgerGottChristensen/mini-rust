use std::fmt::{Debug, Formatter};
use proc_macro2::Ident;
use syn::punctuated::Punctuated;
use syn::{braced, Field, Token, Type, ItemStruct, WhereClause};
use syn::parse::{Parse, Parser, ParseStream};
use syn::token::{Brace, Colon, Comma, Struct};
use crate::{MiniGenerics, MiniIdent, MiniType};

#[derive(PartialEq, Clone)]
pub struct MiniStruct {
    pub struct_token: Struct,
    pub ident: MiniIdent,
    pub generics: MiniGenerics,
    pub brace: Brace,
    pub fields: Punctuated<MiniStructField, Comma>
}

impl Debug for MiniStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MiniStruct")
            .field("ident", &self.ident)
            .field("generics", &self.generics)
            .field("fields", &self.fields.iter().collect::<Vec<_>>())
            .finish()
    }
}

impl Parse for MiniStruct {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let struct_token = input.parse::<Token![struct]>()?;
        let ident = MiniIdent::parse(input)?;
        let generics = MiniGenerics::parse(input)?;
        let (where_clause, brace, fields) = data_struct(input)?;


        Ok(MiniStruct {
            struct_token,
            ident,
            generics: MiniGenerics {
                where_clause,
                ..generics
            },
            brace,
            fields
        })
    }
}

pub fn data_struct(
    input: ParseStream,
) -> syn::Result<(Option<WhereClause>, Brace, Punctuated<MiniStructField, Comma>)> {
    let mut lookahead = input.lookahead1();
    let mut where_clause = None;
    if lookahead.peek(Token![where]) {
        where_clause = Some(input.parse()?);
        lookahead = input.lookahead1();
    }

    let content;

    let brace =  braced!(content in input);
    let fields = content.parse_terminated(MiniStructField::parse)?;

    Ok((where_clause, brace, fields))
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