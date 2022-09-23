use syn::parse::ParseStream;
use syn::Token;
use crate::mini_expr::{AllowStruct, MiniExpr};

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