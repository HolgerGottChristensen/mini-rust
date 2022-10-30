use std::fmt::{Debug, Formatter};

use proc_macro2::Span;
use syn::{Error, Token};
use syn::parse::{Parse, ParseStream};

use mini_ir::{Term, Type};

use crate::{MiniEnum, MiniFn, MiniImpl, MiniStruct, MiniTrait, ToMiniIrTerm, ToMiniIrType};

#[derive(PartialEq, Clone)]
pub enum MiniItem {
    Enum(MiniEnum),
    Fn(MiniFn),
    Impl(MiniImpl),
    Struct(MiniStruct),
    Trait(MiniTrait),
}

impl Debug for MiniItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MiniItem::Enum(s) => s.fmt(f),
            MiniItem::Fn(s) => s.fmt(f),
            MiniItem::Impl(s) => s.fmt(f),
            MiniItem::Struct(s) => s.fmt(f),
            MiniItem::Trait(s) => s.fmt(f),
        }
    }
}

impl Parse for MiniItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![struct]) {
            return Ok(MiniItem::Struct(MiniStruct::parse(input)?));
        } else if input.peek(Token!(enum)) {
            return Ok(MiniItem::Enum(MiniEnum::parse(input)?));
        } else if input.peek(Token!(fn)) {
            return Ok(MiniItem::Fn(MiniFn::parse(input)?));
        } else if input.peek(Token!(impl)) {
            return Ok(MiniItem::Impl(MiniImpl::parse(input)?));
        } else if input.peek(Token!(trait)) {
            return Ok(MiniItem::Trait(MiniTrait::parse(input)?));
        }

        Err(Error::new(Span::call_site(), "Could not parse mini-item"))
    }
}

impl ToMiniIrType for MiniItem {
    fn convert_type(&self) -> Type {
        match self {
            MiniItem::Enum(f) => f.convert_type(),
            MiniItem::Fn(f) => f.convert_type(),
            MiniItem::Impl(_) => todo!(),
            MiniItem::Struct(s) => s.convert_type(),
            MiniItem::Trait(_) => todo!(),
        }
    }
}

impl ToMiniIrTerm for MiniItem {
    fn convert_term(&self) -> Term {
        match self {
            MiniItem::Enum(f) => f.convert_term(),
            MiniItem::Fn(f) => Term::Let(f.ident.to_string(), Box::new(f.convert_term()), Box::new(Term::Replacement)),
            MiniItem::Impl(f) => f.convert_term(),
            MiniItem::Struct(f) => f.convert_term(),
            MiniItem::Trait(f) => f.convert_term(),
        }
    }
}