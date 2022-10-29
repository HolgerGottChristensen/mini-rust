use mini_ir::Term;

use crate::mini_expr::{MiniBinOp, MiniExpr};
use crate::ToMiniIrTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprBinary {
    pub left: Box<MiniExpr>,
    pub op: MiniBinOp,
    pub right: Box<MiniExpr>,
}

impl ToMiniIrTerm for MiniExprBinary {
    fn convert_term(&self) -> Term {
        let function_name = self.op.name();

        Term::app(Term::app(Term::app(Term::TermVar(function_name), Term::Unit), self.left.convert_term()), self.right.convert_term())
    }
}