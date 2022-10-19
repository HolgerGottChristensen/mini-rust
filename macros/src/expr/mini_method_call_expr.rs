use proc_macro2::Ident;
use syn::{Token, token};
use syn::punctuated::Punctuated;
use system_f_omega::{Term, type_of};
use crate::mini_expr::MiniExpr;
use crate::{MiniIdent, ToSystemFOmegaTerm};

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprMethodCall {
    pub receiver: Box<MiniExpr>,
    pub dot_token: Token![.],
    pub method: MiniIdent,
    //pub turbofish: Option<MethodTurbofish>,
    pub paren_token: token::Paren,
    pub args: Punctuated<MiniExpr, Token![,]>,
}

impl ToSystemFOmegaTerm for MiniExprMethodCall {
    fn convert_term(&self) -> Term {
        todo!()
    }
}