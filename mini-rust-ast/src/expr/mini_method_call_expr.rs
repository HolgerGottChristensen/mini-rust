use syn::{Token, token};
use syn::punctuated::Punctuated;

use mini_ir::Term;

use crate::{MiniIdent, ToMiniIrTerm};
use crate::mini_expr::MiniExpr;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprMethodCall {
    pub receiver: Box<MiniExpr>,
    pub dot_token: Token![.],
    pub method: MiniIdent,
    //pub turbofish: Option<MethodTurbofish>,
    pub paren_token: token::Paren,
    pub args: Punctuated<MiniExpr, Token![,]>,
}

impl ToMiniIrTerm for MiniExprMethodCall {
    fn convert_term(&self) -> Term {
        todo!()
    }
}