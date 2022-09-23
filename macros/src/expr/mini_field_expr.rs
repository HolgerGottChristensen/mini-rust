use syn::{Member, Token};
use crate::mini_expr::MiniExpr;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprField {
    pub base: Box<MiniExpr>,
    pub dot_token: Token![.],
    pub member: Member,
}