use syn::token;
use system_f_omega::Term;
use crate::mini_expr::MiniExpr;
use crate::ToSystemFOmegaTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprParen {
    pub paren_token: token::Paren,
    pub expr: Box<MiniExpr>,
}

impl ToSystemFOmegaTerm for MiniExprParen {
    fn convert_term(&self) -> Term {
        self.expr.convert_term()
    }
}