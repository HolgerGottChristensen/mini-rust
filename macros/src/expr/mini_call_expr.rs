use syn::punctuated::Punctuated;
use syn::{token, Token};
use crate::mini_expr::MiniExpr;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprCall {
    pub func: Box<MiniExpr>,
    pub paren_token: token::Paren,
    pub args: Punctuated<MiniExpr, Token![,]>,
}