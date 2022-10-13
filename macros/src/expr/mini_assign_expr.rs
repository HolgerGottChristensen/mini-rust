use syn::{token};
use crate::mini_expr::MiniExpr;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprAssign {
    pub left: Box<MiniExpr>, // Todo: Consider mini-rust scope and maybe convert to ident
    pub eq_token: token::Eq,
    pub right: Box<MiniExpr>,
}