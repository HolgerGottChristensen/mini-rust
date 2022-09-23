use proc_macro2::{Ident, Span};
use syn::punctuated::Punctuated;
use syn::{Error, ItemFn, parenthesized, Pat, PatOr, token, Token, Type};
use syn::parse::{Parse, ParseBuffer, ParseStream};
use syn::parse::discouraged::Speculative;
use syn::token::{And, Colon, Comma, Mut, Paren, RArrow, SelfValue};
use crate::mini_pat::multi_pat;
use crate::stmt::MiniBlock;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniFn {
    pub fn_token: token::Fn,
    pub ident: Ident,
    // pub generics: Generics,
    pub paren_token: Paren,
    pub inputs: Punctuated<MiniFnArg, Comma>,
    pub arrow_token: RArrow,
    pub return_type: Box<Type>,
    pub block: Box<MiniBlock>,
}


#[derive(PartialEq, Clone, Debug)]
pub enum MiniFnArg {
    Receiver {
        reference: Option<And>,
        mutability: Option<Mut>,
        self_token: SelfValue,
    },
    Typed {
        pat: Box<Pat>,
        colon_token: Colon,
        ty: Box<Type>,
    }
}

impl MiniFn {
    fn parse_fn_args(input: ParseStream) -> syn::Result<Punctuated<MiniFnArg, Token![,]>> {
        let mut args = Punctuated::new();
        let mut has_receiver = false;

        while !input.is_empty() {
            let mut arg: MiniFnArg = input.parse()?;
            match &mut arg {
                MiniFnArg::Receiver {..} if has_receiver => {
                    return Err(Error::new(
                        Span::call_site(),
                        "unexpected second method receiver",
                    ));
                }
                MiniFnArg::Receiver {..} if !args.is_empty() => {
                    return Err(Error::new(
                        Span::call_site(),
                        "unexpected method receiver",
                    ));
                }
                MiniFnArg::Receiver {..} => {
                    has_receiver = true;
                }
                MiniFnArg::Typed {..} => (),
            }
            args.push_value(arg);

            if input.is_empty() {
                break;
            }

            let comma: Token![,] = input.parse()?;
            args.push_punct(comma);
        }

        Ok(args)
    }
}

impl Parse for MiniFn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fn_token: Token![fn] = input.parse()?;
        let ident: Ident = input.parse()?;
        //let mut generics: Generics = input.parse()?;

        let content;
        let paren_token = parenthesized!(content in input);
        let mut inputs = MiniFn::parse_fn_args(&content)?;

        let arrow_token = RArrow::parse(input)?;
        let output_ty = Type::parse(input)?;
        //generics.where_clause = input.parse()?;

        Ok(MiniFn {
            fn_token,
            ident,
            paren_token,
            inputs,
            arrow_token,
            return_type: Box::new(output_ty),
            block: Box::new(input.parse()?)
        })
    }
}

impl Parse for MiniFnArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ahead = input.fork();

        fn parse_receiver(input: ParseStream) -> syn::Result<(Option<And>, Option<Mut>, SelfValue)> {
            let and: Option<And> = input.parse()?;
            let mutability: Option<Mut> = input.parse()?;
            let self_token: SelfValue = input.parse()?;
            Ok((and, mutability, self_token))
        }

        fn parse_typed(input: ParseStream) -> syn::Result<(Pat, Colon, Type)> {
            let pat = multi_pat(input)?;
            let colon_token: Colon = input.parse()?;
            let ty: Type = input.parse()?;

            Ok((pat, colon_token, ty))
        }

        if let Ok((reference, mutability, self_token)) = ahead.call(parse_receiver) {
            if !ahead.peek(Token![:]) {
                input.advance_to(&ahead);
                return Ok(MiniFnArg::Receiver {
                    reference,
                    mutability,
                    self_token
                });
            }
        }

        let (pat, colon, ty) = input.call(parse_typed)?;
        Ok(MiniFnArg::Typed {
            pat: Box::new(pat),
            colon_token: colon,
            ty: Box::new(ty)
        })
    }
}