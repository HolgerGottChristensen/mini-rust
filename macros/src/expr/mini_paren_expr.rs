use syn::token;
use crate::mini_expr::MiniExpr;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprParen {
    pub paren_token: token::Paren,
    pub expr: Box<MiniExpr>,
}