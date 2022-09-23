use syn::punctuated::Punctuated;
use syn::{token, Token};
use crate::mini_expr::MiniExpr;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprTuple {
    pub paren_token: token::Paren,
    pub elems: Punctuated<MiniExpr, Token![,]>,
}