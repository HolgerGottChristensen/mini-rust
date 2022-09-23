use syn::{braced, Pat, Token, token};
use syn::parse::{Parse, ParseStream};
use crate::mini_expr::MiniExpr;
use crate::mini_pat::multi_pat_with_leading_vert;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprMatch {
    pub match_token: Token![match],
    pub expr: Box<MiniExpr>,
    pub brace_token: token::Brace,
    pub arms: Vec<MiniArm>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct MiniArm {
    pub pat: Pat,
    pub fat_arrow_token: Token![=>],
    pub body: Box<MiniExpr>,
    pub comma: Option<Token![,]>,
}

impl Parse for MiniExprMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let match_token: Token![match] = input.parse()?;
        let expr = MiniExpr::parse_without_eager_brace(input)?;

        let content;
        let brace_token = braced!(content in input);

        let mut arms = Vec::new();
        while !content.is_empty() {
            arms.push(content.call(MiniArm::parse)?);
        }

        Ok(MiniExprMatch {
            match_token,
            expr: Box::new(expr),
            brace_token,
            arms,
        })
    }
}

impl Parse for MiniArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let requires_comma;
        Ok(MiniArm {
            pat: multi_pat_with_leading_vert(input)?,
            fat_arrow_token: input.parse()?,
            body: {
                let body = input.call(MiniExpr::parse_expr_early)?;
                requires_comma = MiniExpr::requires_terminator(&body);
                Box::new(body)
            },
            comma: {
                if requires_comma && !input.is_empty() {
                    Some(input.parse()?)
                } else {
                    input.parse()?
                }
            },
        })
    }
}