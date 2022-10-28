use mini_ir::Term;

use crate::mini_expr::{MiniBinOp, MiniExpr};
use crate::ToSystemFOmegaTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprBinary {
    pub left: Box<MiniExpr>,
    pub op: MiniBinOp,
    pub right: Box<MiniExpr>,
}

impl ToSystemFOmegaTerm for MiniExprBinary {
    fn convert_term(&self) -> Term {
        let function_name = self.op.name();

        Term::TermApp(
            Box::new(Term::TermApp(
                Box::new(Term::TermVar(function_name)),
                Box::new(self.left.convert_term())
            )),
            Box::new(self.right.convert_term())
        )
    }
}