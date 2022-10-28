use std::fmt::{Debug, Formatter};

use quote::ToTokens;
use syn::{Pat, PatType, Token, token, Type};
use syn::parse::{Parse, ParseStream};
use syn::parse::discouraged::Speculative;
use syn::token::{Let, Semi};

use mini_ir::Term;

use crate::{MiniItem, ToMiniIrTerm, ToMiniIrType};
use crate::mini_expr::MiniExpr;
use crate::mini_pat::{MiniPat, multi_pat_with_leading_vert};

#[derive(PartialEq, Clone)]
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

impl Debug for MiniStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MiniStmt::Local { let_token, pat, eq_token, expr, semi_token } => {
                f.debug_struct("Local")
                    .field("pat", &MiniPat(pat.clone()))
                    .field("expr", &expr)
                    .finish()
            }
            MiniStmt::Item(i) => {
                f.debug_tuple("Item")
                    .field(&i)
                    .finish()
            }
            MiniStmt::Expr(i) => {
                i.fmt(f)
            }
            MiniStmt::Semi(i, _) => {
                f.debug_tuple("Semi")
                    .field(&i)
                    .finish()
            }
        }
    }
}

impl Parse for MiniStmt {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_stmt(input, true)
    }
}

pub fn parse_stmt(input: ParseStream, allow_nosemi: bool) -> syn::Result<MiniStmt> {
    if input.peek(Token![let]) {
        stmt_local(input)
    } else if input.peek(Token![struct]) ||
        input.peek(Token![enum]) ||
        input.peek(Token![fn]) ||
        input.peek(Token!(impl))
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


impl ToMiniIrTerm for MiniStmt {
    fn convert_term(&self) -> Term {
        match self {
            MiniStmt::Local { let_token, pat, eq_token, expr, semi_token } => {
                let mut body = expr.convert_term();

                // Always continue with unit, which will be replaced when having more statements in a row.
                body = Term::Let(
                    pat.to_token_stream().to_string(),
                    Box::new(body),
                    Box::new(Term::Unit),
                );

                body
            }
            MiniStmt::Item(i) => {
                match i {
                    MiniItem::Enum(e) => e.convert_term(),
                    MiniItem::Struct(s) => s.convert_term(),
                    MiniItem::Fn(f) => {
                        Term::Let(f.ident.0.to_string(), Box::new(f.convert_term()), Box::new(Term::Unit))
                    }
                    MiniItem::Impl(i) => i.convert_term(),
                    _ => todo!()
                }
            }
            MiniStmt::Expr(e) => e.convert_term(),
            MiniStmt::Semi(expr, _) => {
                let mut body = expr.convert_term();

                // Always continue with unit, which will be replaced when having more statements in a row.
                body = Term::Seq(
                    Box::new(body),
                    Box::new(Term::Unit),
                );

                body
            }
        }
    }
}

mod tests {
    use syn::parse_quote;

    use mini_ir::{Context, Substitutions, type_of};

    use crate::{MiniStmt, ToMiniIrTerm};

    #[test]
    fn parse_local_simple() {
        // Arrange
        let mini: MiniStmt = parse_quote!(
            let i = 0;
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }
}