use std::fmt::{Debug, Formatter};
use paris::formatter::colorize_string;
use syn::{braced, ItemImpl, Path, Token, Type, TypePath};
use syn::parse::{Parse, ParseStream};
use syn::token::{Brace, For, Impl};
use system_f_omega::{Kind, Term};
use system_f_omega::Type::{TypeApp, TypeVar};
use crate::{MiniFn, MiniGenerics, MiniType, ToSystemFOmegaTerm};
use crate::mini_path::MiniPath;

#[derive(PartialEq, Clone)]
pub struct MiniImpl {
    pub impl_token: Impl,
    pub generics: MiniGenerics,
    pub trait_: Option<(Path, For)>,
    pub self_ty: Box<Type>,
    pub brace_token: Brace,
    pub items: Vec<MiniFn>,
}

impl Debug for MiniImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct(stringify!(MiniImpl));

        if let Some((path, f)) = &self.trait_ {
            s.field("trait", &MiniPath(path.clone()));
        }

        s.field("generics", &self.generics);

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

    let has_generics = input.peek(Token![<])
        && (input.peek2(Token![>])
        || input.peek2(Token![#])
        || input.peek2(syn::Ident)
        && (input.peek3(Token![:])
        || input.peek3(Token![,])
        || input.peek3(Token![>]))
    );

    let mut generics: MiniGenerics = if has_generics {
        input.parse()?
    } else {
        MiniGenerics::default()
    };

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

    generics.where_clause = input.parse()?;

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
            generics,
            trait_,
            self_ty: Box::new(self_ty),
            brace_token,
            items,
        }))
    }
}

impl ToSystemFOmegaTerm for MiniImpl {
    fn convert_term(&self) -> Term {
        let name = MiniType(*self.self_ty.clone()).path().as_ident();
        let generics = MiniType(*self.self_ty.clone()).path().generics();

        let mut body = Term::Unit;

        for item in self.items.iter().rev() {

            let mut fun = item.convert_term();

            // Todo: Is this the best way. We need to introduce generics somehow for each method.
            for param in &self.generics.params {
                fun = Term::TermTypeAbs(param.ident.to_string(), Kind::KindStar, Box::new(fun));
            }

            body = Term::Let(format!("{}::{}", &name, item.ident.to_string()), Box::new(fun), Box::new(body));
        }

        let mut self_type = TypeVar(name);

        // For each generic app it to the self type
        for generic in generics {
            self_type = TypeApp(Box::new(self_type), Box::new(generic));
        }

        // Define the self type for use within the body
        Term::Define("#Self".to_string(), self_type, Box::new(body))
    }
}

mod tests {
    use syn::parse_quote;
    use system_f_omega::{Context, kind_of, type_of};
    use crate::item::MiniImpl;
    use crate::mini_expr::MiniExpr;
    use crate::mini_file::MiniFile;
    use crate::stmt::MiniBlock;
    use crate::ToSystemFOmegaTerm;

    #[test]
    fn simple_impl() {
        // Arrange
        let mini: MiniImpl = parse_quote!(
            impl Test {

            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone());
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn simple_impl_fn() {
        // Arrange
        let mini: MiniImpl = parse_quote!(
            impl Test {
                fn hello(self) -> Test {
                    self
                }
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone());
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn impl_fn_generics() {
        // Arrange
        let mini: MiniImpl = parse_quote!(
            impl<T> Test<T> {
                fn hello(self) -> Test<T> {
                    self
                }
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone());
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn impl_fn_multiple() {
        // Arrange
        let mini: MiniImpl = parse_quote!(
            impl Test {
                fn hello(self) -> Test {
                    self
                }

                fn hello2(t: i64, q: bool) -> bool {
                    q
                }

                fn hello3(v: bool) -> (bool, bool) {
                    (v, v)
                }
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone());
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn simple_struct_impl() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                struct Test {

                }

                impl Test {
                    fn hello(&self) {
                    }
                }

                fn test(t: Test) {
                    Test::hello(&t)
                }
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone());
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }
}