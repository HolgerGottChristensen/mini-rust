use proc_macro2::Ident;
use syn::parse::{Parse, ParseStream};
use crate::Kind;

syn::custom_keyword!(lambda);

pub enum Type {
    TypeVara,
    TypeVar(Ident),
    TypeArrow(Box<Type>, Box<Type>),
    TypeAbs(Ident, Kind, Box<Type>),
    TypeApp(Box<Type>, Box<Type>),
    TypeAll(Ident, Kind, Box<Type>),
}
