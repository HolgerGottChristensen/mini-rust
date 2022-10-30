use syn::token;

use mini_ir::Term;

use crate::mini_expr::MiniExpr;
use crate::ToMiniIrTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprParen {
    pub paren_token: token::Paren,
    pub expr: Box<MiniExpr>,
}

impl ToMiniIrTerm for MiniExprParen {
    fn convert_term(&self) -> Term {
        self.expr.convert_term()
    }
}