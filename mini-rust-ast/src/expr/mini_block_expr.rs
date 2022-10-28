use syn::braced;
use syn::parse::{Parse, ParseStream};
use mini_ir::Term;
use crate::stmt::{MiniBlock, parse_within};
use crate::ToSystemFOmegaTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprBlock {
    pub block: MiniBlock,
}

impl Parse for MiniExprBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {

        let content;
        let brace_token = braced!(content in input);

        let stmts = content.call(parse_within)?;

        Ok(MiniExprBlock {
            block: MiniBlock { brace_token, stmts },
        })
    }
}

impl ToSystemFOmegaTerm for MiniExprBlock {
    fn convert_term(&self) -> Term {
        self.block.convert_term()
    }
}