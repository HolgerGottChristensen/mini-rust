use syn::{token};
use crate::mini_expr::MiniExpr;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprAssign {
    pub left: Box<MiniExpr>,
    pub eq_token: token::Eq,
    pub right: Box<MiniExpr>,
}