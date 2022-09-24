use std::fmt::{Debug, Formatter};
use proc_macro2::Ident;
use syn::punctuated::Punctuated;
use syn::{braced, ItemTrait, Path, token, Token};
use syn::parse::{Parse, ParseStream};
use syn::token::{Add, Brace, Colon, Trait};
use crate::{MiniFn, MiniIdent};
use crate::mini_path::MiniPath;

#[derive(PartialEq, Clone)]
pub struct MiniTrait {
    pub trait_token: Trait,
    pub ident: MiniIdent,
    //pub generics: Generics,
    pub colon_token: Option<Colon>,
    pub super_traits: Punctuated<TraitBound, Add>,
    pub brace_token: Brace,
    pub items: Vec<MiniFn>,
}

#[derive(PartialEq, Clone)]
pub struct TraitBound {
    pub paren_token: Option<token::Paren>,
    pub path: Path,
}

impl Debug for MiniTrait {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("MiniTrait");

        s.field("ident", &self.ident);

        if self.super_traits.len() > 0 {
            s.field("super_traits", &self.super_traits.iter().collect::<Vec<_>>());
        }

        s.field("items", &self.items);

        s.finish()
    }
}

impl Parse for MiniTrait {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let trait_token: Token![trait] = input.parse()?;
        let ident: MiniIdent = input.parse()?;
        //let generics: Generics = input.parse()?;
        parse_rest_of_trait(
            input,
            trait_token,
            ident,
            //generics,
        )
    }
}

fn parse_rest_of_trait(
    input: ParseStream,
    trait_token: Token![trait],
    ident: MiniIdent,
    //mut generics: Generics,
) -> syn::Result<MiniTrait> {
    let colon_token: Option<Token![:]> = input.parse()?;

    let mut super_traits = Punctuated::new();
    if colon_token.is_some() {
        loop {
            if input.peek(Token![where]) || input.peek(token::Brace) {
                break;
            }
            super_traits.push_value(input.parse()?);
            if input.peek(Token![where]) || input.peek(token::Brace) {
                break;
            }
            super_traits.push_punct(input.parse()?);
        }
    }

    //generics.where_clause = input.parse()?;

    let content;
    let brace_token = braced!(content in input);

    let mut items = Vec::new();
    while !content.is_empty() {
        items.push(content.parse()?);
    }

    Ok(MiniTrait {
        trait_token,
        ident,
        colon_token,
        super_traits,
        brace_token,
        items,
    })
}

impl Debug for TraitBound {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        MiniPath(self.path.clone()).fmt(f)
    }
}

impl Parse for TraitBound {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        /*let tilde_const = if input.peek(Token![~]) && input.peek2(Token![const]) {
            let tilde_token = input.parse::<Token![~]>()?;
            let const_token = input.parse::<Token![const]>()?;
            Some((tilde_token, const_token))
        } else {
            None
        };*/

        let mut path: Path = input.parse()?;
        /*if path.segments.last().unwrap().arguments.is_empty()
            && (input.peek(token::Paren) || input.peek(Token![::]) && input.peek3(token::Paren))
        {
            input.parse::<Option<Token![::]>>()?;
            let args: ParenthesizedGenericArguments = input.parse()?;
            let parenthesized = PathArguments::Parenthesized(args);
            path.segments.last_mut().unwrap().arguments = parenthesized;
        }


        {
            if let Some((tilde_token, const_token)) = tilde_const {
                path.segments.insert(
                    0,
                    PathSegment {
                        ident: Ident::new("const", const_token.span),
                        arguments: PathArguments::None,
                    },
                );
                let (_const, punct) = path.segments.pairs_mut().next().unwrap().into_tuple();
                *punct.unwrap() = Token![::](tilde_token.span);
            }
        }*/

        Ok(TraitBound {
            paren_token: None,
            path,
        })
    }
}