use std::fmt::{Debug, Formatter};

use syn::{braced, token};
use syn::parse::{Parse, ParseStream};

use mini_ir::Term;

use crate::{MiniStmt, parse_stmt, ToMiniIrTerm};

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

impl ToMiniIrTerm for MiniBlock {
    fn convert_term(&self) -> Term {
        // Check if there are any statements. If not, return unit.
        if self.stmts.len() > 0 {
            // Todo: Because rust is weird we should loop all Stmt::Item first and then all other. https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=1892de5eee6ea0f511551a3a647366b5
            // Todo: But not in the case where the Item is last: https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=eed9e46426ec7c4a2804ba26e1d124ac
            // Todo: Personally I think we should do the first, and note the other in the report.
            let mut body = self.stmts[self.stmts.len() - 1].convert_term();

            fn replace_inner(term: Term, with: Term) -> Term {
                match term {
                    Term::Define(x, ty, inner) => {
                        Term::Define(x, ty, Box::new(replace_inner(*inner, with)))
                    }
                    Term::Let(a, ty, inner) => {
                        Term::Let(a, ty, Box::new(replace_inner(*inner, with)))
                    }
                    Term::Unit => {
                        with
                    }
                    Term::Seq(term1, term2) => {
                        Term::Seq(term1, Box::new(replace_inner(*term2, with)))
                    }
                    Term::TermTypeAbs(s, k, inner) => {
                        Term::TermTypeAbs(s, k, Box::new(replace_inner(*inner, with)))
                    }
                    a => panic!("Expressions need to be the last in the block")
                }
            }

            for stmt in self.stmts.iter().rev().skip(1) {
                body = replace_inner(stmt.convert_term(), body);
            }

            // Todo: Check if only the last statement and returns are non-unit.

            // Because of scoping we need to hide all things from the body.
            Term::Scope(Box::new(body))
        } else {
            Term::Unit
        }
    }
}