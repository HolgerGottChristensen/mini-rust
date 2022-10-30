use std::fmt::{Debug, Formatter};

use proc_macro2::Ident;
use syn::{Path, PathArguments, PathSegment, QSelf, Token, token, Type};
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

use mini_ir::Term;

use crate::mini_path::MiniPath;
use crate::ToMiniIrTerm;

#[derive(PartialEq, Clone)]
pub struct MiniExprPath {
    pub qself: Option<QSelf>,
    pub path: Path,
}

impl Debug for MiniExprPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("MiniExprPath");

        if let Some(se) = &self.qself {
            s.field("self", se);
        }

        s.field("path", &MiniPath(self.path.clone()));
        s.finish()
    }
}

impl Parse for MiniExprPath {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let (qself, path) = qpath(input, true)?;

        Ok(MiniExprPath { qself, path })
    }
}

pub fn qpath(input: ParseStream, expr_style: bool) -> syn::Result<(Option<QSelf>, Path)> {
    if input.peek(Token![<]) {
        let lt_token: Token![<] = input.parse()?;
        let this: Type = input.parse()?;
        let path = if input.peek(Token![as]) {
            let as_token: Token![as] = input.parse()?;
            let path: Path = input.parse()?;
            Some((as_token, path))
        } else {
            None
        };
        let gt_token: Token![>] = input.parse()?;
        let colon2_token: Token![::] = input.parse()?;
        let mut rest = Punctuated::new();
        loop {
            let path = parse_segment_helper(input, expr_style)?;
            rest.push_value(path);
            if !input.peek(Token![::]) {
                break;
            }
            let punct: Token![::] = input.parse()?;
            rest.push_punct(punct);
        }
        let (position, as_token, path) = match path {
            Some((as_token, mut path)) => {
                let pos = path.segments.len();
                path.segments.push_punct(colon2_token);
                path.segments.extend(rest.into_pairs());
                (pos, Some(as_token), path)
            }
            None => {
                let path = Path {
                    leading_colon: Some(colon2_token),
                    segments: rest,
                };
                (0, None, path)
            }
        };
        let qself = QSelf {
            lt_token,
            ty: Box::new(this),
            position,
            as_token,
            gt_token,
        };
        Ok((Some(qself), path))
    } else {
        let path = parse_path_helper(input, expr_style)?;
        Ok((None, path))
    }
}

fn parse_path_helper(input: ParseStream, expr_style: bool) -> syn::Result<Path> {
    let mut path = Path {
        leading_colon: input.parse()?,
        segments: {
            let mut segments = Punctuated::new();
            let value = parse_segment_helper(input, expr_style)?;
            segments.push_value(value);
            segments
        },
    };
    parse_path_rest(input, &mut path, expr_style)?;
    Ok(path)
}

fn parse_path_rest(
    input: ParseStream,
    path: &mut Path,
    expr_style: bool,
) -> syn::Result<()> {
    while input.peek(Token![::]) && !input.peek3(token::Paren) {
        let punct: Token![::] = input.parse()?;
        path.segments.push_punct(punct);
        let value = parse_segment_helper(input, expr_style)?;
        path.segments.push_value(value);
    }
    Ok(())
}

fn parse_segment_helper(input: ParseStream, expr_style: bool) -> syn::Result<PathSegment> {
    if input.peek(Token![super]) || input.peek(Token![self]) || input.peek(Token![crate]) {
        let ident = input.call(Ident::parse_any)?;
        return Ok(PathSegment::from(ident));
    }

    let ident = if input.peek(Token![Self]) {
        input.call(Ident::parse_any)?
    } else {
        input.parse()?
    };

    if !expr_style && input.peek(Token![<]) && !input.peek(Token![<=])
        || input.peek(Token![::]) && input.peek3(Token![<])
    {
        Ok(PathSegment {
            ident,
            arguments: PathArguments::AngleBracketed(input.parse()?),
        })
    } else {
        Ok(PathSegment::from(ident))
    }
}

impl ToMiniIrTerm for MiniExprPath {
    fn convert_term(&self) -> Term {
        // Todo: What about self?
        let mut body = Term::TermVar(MiniPath(self.path.clone()).as_ident());

        for generic in MiniPath(self.path.clone()).generics() {
            body = Term::TermTypeApp(Box::new(body), generic);
        }

        body
    }
}