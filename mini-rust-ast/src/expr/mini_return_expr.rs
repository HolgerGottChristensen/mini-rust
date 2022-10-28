use syn::parse::ParseStream;
use syn::Token;

use mini_ir::{Term, Type};

use crate::mini_expr::{AllowStruct, MiniExpr};
use crate::ToSystemFOmegaTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprReturn {
    pub return_token: Token![return],
    pub expr: Box<MiniExpr>,
}

pub fn expr_ret(input: ParseStream, allow_struct: AllowStruct) -> syn::Result<MiniExprReturn> {
    Ok(MiniExprReturn {
        return_token: input.parse()?,
        expr: Box::new(MiniExpr::parse_ambiguous_expr(input, allow_struct)?),
    })
}

impl ToSystemFOmegaTerm for MiniExprReturn {
    fn convert_term(&self) -> Term {
        Term::Ascribe(Box::new(self.expr.convert_term()), Type::TypeVar("#Return".to_string()))
    }
}