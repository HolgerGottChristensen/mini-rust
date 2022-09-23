use syn::parse::ParseStream;
use crate::mini_expr::{AllowStruct, MiniExpr, MiniUnOp};

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprUnary {
    pub op: MiniUnOp,
    pub expr: Box<MiniExpr>,
}


pub fn parse_expr_unary(
    input: ParseStream,
    allow_struct: AllowStruct,
) -> syn::Result<MiniExprUnary> {
    Ok(MiniExprUnary {
        op: input.parse()?,
        expr: Box::new(MiniExpr::parse_unary_expr(input, allow_struct)?),
    })
}