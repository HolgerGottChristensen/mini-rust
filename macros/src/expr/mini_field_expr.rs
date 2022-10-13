use syn::{Member, Token};
use system_f_omega::Term;
use crate::mini_expr::MiniExpr;
use crate::ToSystemFOmegaTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprField {
    pub base: Box<MiniExpr>,
    pub dot_token: Token![.],
    pub member: Member,
}

impl ToSystemFOmegaTerm for MiniExprField {
    fn convert_term(&self) -> Term {
        match &self.member {
            Member::Named(named) => {
                Term::RecordProjection(Box::new(self.base.convert_term()), named.to_string())
            }
            Member::Unnamed(index) => {
                Term::TupleProjection(Box::new(self.base.convert_term()), index.index as usize)
            }
        }
    }
}