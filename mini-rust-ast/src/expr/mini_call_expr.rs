use syn::punctuated::Punctuated;
use syn::{token, Token};
use mini_ir::Term;
use crate::mini_expr::MiniExpr;
use crate::ToSystemFOmegaTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprCall {
    pub func: Box<MiniExpr>,
    pub paren_token: token::Paren,
    pub args: Punctuated<MiniExpr, Token![,]>,
}

impl ToSystemFOmegaTerm for MiniExprCall {
    fn convert_term(&self) -> Term {
        let mut body = self.func.convert_term();

        // Because we always start with a unit, we apply the unit first.
        body = Term::TermApp(Box::new(body), Box::new(Term::Unit));

        for arg in self.args.iter() {
            body = Term::TermApp(Box::new(body), Box::new(arg.convert_term()));
        }

        body
    }
}