use proc_macro2::Ident;
use syn::parse::{ParseBuffer, ParseStream};
use syn::{Pat, Path, PatType, Token, token, Type};
use syn::parse::discouraged::Speculative;
use syn::token::{Let, Semi};
use crate::mini_expr::MiniExpr;
use crate::mini_pat::multi_pat_with_leading_vert;
use crate::MiniItem;

#[derive(PartialEq, Clone, Debug)]
pub enum MiniStmt {
    Local {
        let_token: Let,
        pat: Pat,
        eq_token: token::Eq,
        expr: Box<MiniExpr>,
        semi_token: Semi,
    },
    Item(MiniItem),
    Expr(MiniExpr),
    Semi(MiniExpr, Semi),
}

pub fn parse_stmt(input: ParseStream, allow_nosemi: bool) -> syn::Result<MiniStmt> {
    if input.peek(Token![let]) {
        stmt_local(input)
    } else if input.peek(Token![struct]) ||
        input.peek(Token![enum]) ||
        input.peek(Token![fn])
    {
        let mut item: MiniItem = input.parse()?;
        Ok(MiniStmt::Item(item))
    } else {
        stmt_expr(input, allow_nosemi)
    }
}

fn stmt_local(input: ParseStream) -> syn::Result<MiniStmt> {
    let let_token: Token![let] = input.parse()?;

    let mut pat: Pat = multi_pat_with_leading_vert(input)?;
    if input.peek(Token![:]) {
        let colon_token: Token![:] = input.parse()?;
        let ty: Type = input.parse()?;
        pat = Pat::Type(PatType {
            attrs: Vec::new(),
            pat: Box::new(pat),
            colon_token,
            ty: Box::new(ty),
        });
    }

    let eq_token: Token![=] = input.parse()?;
    let expr: MiniExpr = input.parse()?;
    let semi_token: Token![;] = input.parse()?;

    Ok(MiniStmt::Local {
        let_token,
        pat,
        eq_token,
        expr: Box::new(expr),
        semi_token,
    })
}

fn stmt_expr(
    input: ParseStream,
    allow_nosemi: bool,
) -> syn::Result<MiniStmt> {
    let mut e = MiniExpr::parse_expr_early(input)?;

    if input.peek(Token![;]) {
        return Ok(MiniStmt::Semi(e, input.parse()?));
    }

    if allow_nosemi || !e.requires_terminator() {
        Ok(MiniStmt::Expr(e))
    } else {
        Err(input.error("expected semicolon"))
    }
}