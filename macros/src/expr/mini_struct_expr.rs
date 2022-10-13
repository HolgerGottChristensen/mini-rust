use std::collections::HashMap;
use std::iter::once;
use quote::ToTokens;
use syn::{Attribute, braced, Expr, ExprPath, FieldValue, Member, Path, token, Token};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use system_f_omega::{BaseType, Term, Type};
use crate::mini_expr::MiniExpr;
use crate::{MiniExprPath, MiniIdent, ToSystemFOmegaTerm};
use crate::mini_path::MiniPath;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprStruct {
    pub path: MiniPath,
    pub brace_token: token::Brace,
    pub fields: Punctuated<MiniFieldValue, Token![,]>,
    pub dot2_token: Option<Token![..]>,
    pub rest: Option<Box<MiniExpr>>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct MiniFieldValue {
    pub name: MiniIdent,
    pub colon_token: Token![:],
    pub expr: MiniExpr,
}

impl Parse for MiniFieldValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = MiniIdent::parse(input)?;
        let colon_token: Token![:] = input.parse()?;
        let expr = MiniExpr::parse(input)?;

        Ok(MiniFieldValue {
            name: ident,
            colon_token,
            expr
        })
    }
}

pub fn expr_struct_helper(input: ParseStream, path: Path) -> syn::Result<MiniExprStruct> {
    let content;
    let brace_token = braced!(content in input);

    let mut fields = Punctuated::new();
    while !content.is_empty() {
        if content.peek(Token![..]) {
            return Ok(MiniExprStruct {
                brace_token,
                path: MiniPath(path),
                fields,
                dot2_token: Some(content.parse()?),
                rest: if content.is_empty() {
                    None
                } else {
                    Some(Box::new(content.parse()?))
                },
            });
        }

        fields.push(content.parse()?);
        if content.is_empty() {
            break;
        }
        let punct: Token![,] = content.parse()?;
        fields.push_punct(punct);
    }

    Ok(MiniExprStruct {
        brace_token,
        path: MiniPath(path),
        fields,
        dot2_token: None,
        rest: None,
    })
}

impl ToSystemFOmegaTerm for MiniExprStruct {
    fn convert_term(&self) -> Term {
        let fields = self.fields.iter().map(|f| {
            // Todo: Handle paths with generics applied
            (f.name.0.to_string(), f.expr.convert_term())
        });

        let fields =
            fields.chain(once((format!("#{}", self.path.as_ident()), Term::Unit)));

        let map = HashMap::from_iter(fields);

        let mut ascription = Type::TypeVar(self.path.as_ident());

        for generic in self.path.generics() {
            ascription = Type::TypeApp(Box::new(ascription), Box::new(generic));
        }

        Term::Ascribe(Box::new(Term::Record(map)), ascription)
    }
}