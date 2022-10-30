use syn::parse::ParseStream;
use syn::Token;

use crate::mini_expr::{AllowStruct, MiniExpr};

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprBox {
    pub box_token: Token![box],
    pub expr: Box<MiniExpr>,
}

pub fn parse_expr_box(
    input: ParseStream,
    allow_struct: AllowStruct,
) -> syn::Result<MiniExprBox> {
    Ok(MiniExprBox {
        box_token: input.parse()?,
        expr: Box::new(MiniExpr::parse_unary_expr(input, allow_struct)?),
    })
}