use std::fmt::{Debug, Formatter};
use proc_macro2::Span;
use syn::parse::{Parse, ParseStream};
use syn::{Error, Token};
use crate::{MiniEnum, MiniFn, MiniImpl, MiniStruct, MiniTrait};

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
            return Ok(MiniItem::Struct(MiniStruct::parse(input)?))
        } else if input.peek(Token!(enum)) {
            return Ok(MiniItem::Enum(MiniEnum::parse(input)?))
        } else if input.peek(Token!(fn)) {
            return Ok(MiniItem::Fn(MiniFn::parse(input)?))
        } else if input.peek(Token!(impl)) {
            return Ok(MiniItem::Impl(MiniImpl::parse(input)?))
        } else if input.peek(Token!(trait)) {
            return Ok(MiniItem::Trait(MiniTrait::parse(input)?))
        }

        Err(Error::new(Span::call_site(), "Could not parse mini-item"))
    }
}