use proc_macro2::Ident;
use syn::{Token, token};
use syn::punctuated::Punctuated;
use crate::mini_expr::MiniExpr;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprMethodCall {
    pub receiver: Box<MiniExpr>,
    pub dot_token: Token![.],
    pub method: Ident,
    //pub turbofish: Option<MethodTurbofish>,
    pub paren_token: token::Paren,
    pub args: Punctuated<MiniExpr, Token![,]>,
}