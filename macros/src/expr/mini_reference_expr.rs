use syn::Token;
use crate::mini_expr::MiniExpr;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprReference {
    pub and_token: Token![&],
    pub mutability: Option<Token![mut]>,
    pub expr: Box<MiniExpr>,
}