use std::fmt::{Debug, Formatter};

use proc_macro2::Ident;
use syn::{Token, WhereClause};
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

use crate::{MiniIdent, MiniRecIdent, TraitBound};

#[derive(PartialEq, Clone)]
pub struct MiniGenerics {
    pub lt_token: Option<Token![<]>,
    pub params: Punctuated<MiniTypeParam, Token![,]>,
    pub gt_token: Option<Token![>]>,
    pub where_clause: Option<WhereClause>,
}

impl Debug for MiniGenerics {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("MiniGenerics");

        s.field("params", &self.params.iter().collect::<Vec<_>>());

        if let Some(where_clause) = &self.where_clause {
            s.field("where", where_clause);
        }

        s.finish()
    }
}

#[derive(PartialEq, Clone)]
pub struct MiniTypeParam {
    pub ident: MiniRecIdent,
    pub colon_token: Option<Token![:]>,
    pub bounds: Punctuated<TraitBound, Token![+]>,
    //pub eq_token: Option<Token![=]>,
    //pub default: Option<Type>,
}

impl Debug for MiniTypeParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("MiniTypeParam");

        s.field("ident", &self.ident);

        if !self.bounds.is_empty() {
            s.field("bounds", &self.bounds.iter().collect::<Vec<_>>());
        }

        s.finish()
    }
}

impl Default for MiniGenerics {
    fn default() -> Self {
        MiniGenerics {
            lt_token: None,
            params: Punctuated::new(),
            gt_token: None,
            where_clause: None,
        }
    }
}

impl Parse for MiniGenerics {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if !input.peek(Token![<]) {
            return Ok(MiniGenerics::default());
        }

        let lt_token: Token![<] = input.parse()?;

        let mut params = Punctuated::new();
        loop {
            if input.peek(Token![>]) {
                break;
            }

            let lookahead = input.lookahead1();
            if lookahead.peek(syn::Ident) {
                params.push_value(MiniTypeParam::parse(input)?);
            }
            /*
            else if input.peek(Token![_]) {

                params.push_value(MiniTypeParam {
                    ident: MiniIdent(input.call(Ident::parse_any)?),
                    colon_token: None,
                    bounds: Punctuated::new(),
                });
            }
            */
            else {
                return Err(lookahead.error());
            }

            if input.peek(Token![>]) {
                break;
            }
            let punct = input.parse()?;
            params.push_punct(punct);
        }

        let gt_token: Token![>] = input.parse()?;

        Ok(MiniGenerics {
            lt_token: Some(lt_token),
            params,
            gt_token: Some(gt_token),
            where_clause: None,
        })
    }
}

impl Parse for MiniTypeParam {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = MiniRecIdent::parse(input)?;
        let colon_token: Option<Token![:]> = input.parse()?;

        let mut bounds = Punctuated::new();
        if colon_token.is_some() {
            loop {
                if input.peek(Token![,]) || input.peek(Token![>]) || input.peek(Token![=]) {
                    break;
                }

                let value = TraitBound::parse(input)?;
                bounds.push_value(value);
                if !input.peek(Token![+]) {
                    break;
                }
                let punct: Token![+] = input.parse()?;
                bounds.push_punct(punct);
            }
        }

        Ok(MiniTypeParam {
            ident,
            colon_token,
            bounds,
        })
    }
}