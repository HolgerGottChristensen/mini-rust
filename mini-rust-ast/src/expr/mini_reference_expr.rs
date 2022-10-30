use syn::Token;

use mini_ir::Term;

use crate::mini_expr::MiniExpr;
use crate::ToMiniIrTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprReference {
    pub and_token: Token![&],
    pub mutability: Option<Token![mut]>,
    pub expr: Box<MiniExpr>,
}

impl ToMiniIrTerm for MiniExprReference {
    fn convert_term(&self) -> Term {
        Term::Reference(Box::new(self.expr.convert_term())) // todo: Consider mutability
    }
}