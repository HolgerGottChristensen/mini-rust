use std::fmt::{Debug, Formatter};
use syn::{braced, Stmt, token, Token};
use syn::parse::{Parse, ParseStream};
use crate::{MiniStmt, parse_stmt};

#[derive(PartialEq, Clone)]
pub struct MiniBlock {
    pub brace_token: token::Brace,
    /// Statements in a block
    pub stmts: Vec<MiniStmt>,
}

impl Debug for MiniBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MiniBlock")
            .field("stmts", &self.stmts)
            .finish()
    }
}

impl Parse for MiniBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {

        let content;
        Ok(MiniBlock {
            brace_token: braced!(content in input),
            stmts: content.call(parse_within)?,
        })
    }
}

pub fn parse_within(input: ParseStream) -> syn::Result<Vec<MiniStmt>> {
    let mut stmts = Vec::new();
    loop {
        //while let Some(semi) = input.parse::<Option<Token![;]>>()? {
            //stmts.push(Stmt::Semi(Expr::Verbatim(TokenStream::new()), semi));
        //}
        if input.is_empty() {
            break;
        }
        let s = parse_stmt(input, true)?;

        let requires_semicolon = if let MiniStmt::Expr(s) = &s {
            s.requires_terminator()
        } else {
            false
        };

        stmts.push(s);
        if input.is_empty() {
            break;
        } else if requires_semicolon {
            return Err(input.error("unexpected token"));
        }
    }
    Ok(stmts)
}