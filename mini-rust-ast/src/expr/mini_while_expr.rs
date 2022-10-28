use syn::{braced, Token};
use syn::parse::{Parse, ParseStream};

use crate::mini_expr::MiniExpr;
use crate::stmt::{MiniBlock, parse_within};

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprWhile {
    pub while_token: Token![while],
    pub cond: Box<MiniExpr>,
    pub body: MiniBlock,
}

impl Parse for MiniExprWhile {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let while_token: Token![while] = input.parse()?;
        let cond = MiniExpr::parse_without_eager_brace(input)?;

        let content;
        let brace_token = braced!(content in input);
        let stmts = content.call(parse_within)?;

        Ok(MiniExprWhile {
            while_token,
            cond: Box::new(cond),
            body: MiniBlock { brace_token, stmts },
        })
    }
}

