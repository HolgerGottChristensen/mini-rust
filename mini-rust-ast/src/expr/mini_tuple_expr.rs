use syn::{token, Token};
use syn::punctuated::Punctuated;

use mini_ir::Term;

use crate::mini_expr::MiniExpr;
use crate::ToMiniIrTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprTuple {
    pub paren_token: token::Paren,
    pub elems: Punctuated<MiniExpr, Token![,]>,
}

impl ToMiniIrTerm for MiniExprTuple {
    fn convert_term(&self) -> Term {
        if self.elems.iter().count() == 0 {
            Term::Unit
        } else {
            let mut terms = vec![];

            for elem in &self.elems {
                terms.push(elem.convert_term());
            }

            Term::Tuple(terms)
        }
    }
}