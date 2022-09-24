use std::fmt::{Debug, Formatter};
use paris::formatter::colorize_string;
use syn::{braced, ItemImpl, Path, Token, Type, TypePath};
use syn::parse::{Parse, ParseStream};
use syn::token::{Brace, For, Impl};
use crate::{MiniFn, MiniType};

#[derive(PartialEq, Clone)]
pub struct MiniImpl {
    pub impl_token: Impl,
    //pub generics: Generics,
    pub trait_: Option<(Path, For)>,
    pub self_ty: Box<Type>,
    pub brace_token: Brace,
    pub items: Vec<MiniFn>,
}

impl Debug for MiniImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct(stringify!(MiniImpl));

        if let Some((path, f)) = &self.trait_ {
            s.field("trait", path);
        }

        s.field("type", &MiniType((*self.self_ty).clone()));
        s.field("items", &self.items);

        s.finish()
    }
}

impl Parse for MiniImpl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let allow_verbatim_impl = false;
        parse_impl(input, allow_verbatim_impl).map(Option::unwrap)
    }
}

fn parse_impl(input: ParseStream, allow_verbatim_impl: bool) -> syn::Result<Option<MiniImpl>> {
    let impl_token: Token![impl] = input.parse()?;

    /*let has_generics = input.peek(Token![<])
        && (input.peek2(Token![>])
        || input.peek2(Token![#])
        || (input.peek2(Ident) || input.peek2(Lifetime))
        && (input.peek3(Token![:])
        || input.peek3(Token![,])
        || input.peek3(Token![>])
        || input.peek3(Token![=]))
        || input.peek2(Token![const]));

    let mut generics: Generics = if has_generics {
        input.parse()?
    } else {
        Generics::default()
    };*/

    let begin = input.fork();

    let first_ty_span = input.span();
    let mut first_ty: Type = input.parse()?;
    let self_ty: Type;
    let trait_;

    let is_impl_for = input.peek(Token![for]);
    if is_impl_for {
        let for_token: Token![for] = input.parse()?;
        let mut first_ty_ref = &first_ty;
        while let Type::Group(ty) = first_ty_ref {
            first_ty_ref = &ty.elem;
        }
        if let Type::Path(TypePath { qself: None, .. }) = first_ty_ref {
            while let Type::Group(ty) = first_ty {
                first_ty = *ty.elem;
            }
            if let Type::Path(TypePath { qself: None, path }) = first_ty {
                trait_ = Some((path, for_token));
            } else {
                unreachable!();
            }
        } else if !allow_verbatim_impl {
            return Err(syn::Error::new(first_ty_span, "expected trait path"));
        } else {
            trait_ = None;
        }
        self_ty = input.parse()?;
    } else {
        trait_ = None;
        self_ty = first_ty;
    }

    //generics.where_clause = input.parse()?;

    let content;
    let brace_token = braced!(content in input);

    let mut items = Vec::new();
    while !content.is_empty() {
        items.push(content.parse()?);
    }

    if is_impl_for && trait_.is_none() {
        Ok(None)
    } else {
        Ok(Some(MiniImpl {
            impl_token,
            trait_,
            self_ty: Box::new(self_ty),
            brace_token,
            items,
        }))
    }
}