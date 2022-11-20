use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use syn::{braced, Path, Token, Type, TypePath};
use syn::parse::{Parse, ParseStream};
use syn::token::{Brace, For, Impl};

use mini_ir::{Kind, Term};
use mini_ir::Type::{TypeApp, TypeVar};

use crate::{MiniFn, MiniGenerics, MiniType, ToMiniIrKind, ToMiniIrTerm, ToMiniIrType};
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

    generics.where_clause = input.parse().ok();

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

impl ToMiniIrTerm for MiniImpl {
    fn convert_term(&self) -> Term {
        if let Some((p, _)) = &self.trait_ {
            todo!("This requires type classes and is out of bounds.")
        } else {
            let name = MiniType(*self.self_ty.clone()).path().as_ident();
            let generics = MiniType(*self.self_ty.clone()).path().generics();

            let mut body = Term::Replacement;

            for item in self.items.iter().rev() {
                let mut fun = item.convert_term();

                // Todo: Is this the best way. We need to introduce generics somehow for each method.
                for param in &self.generics.params {
                    fun = Term::TermTypeAbs(param.ident.to_string(), param.ident.convert_kind(), Box::new(fun));
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
}

mod tests {


    mod impls {
        use paris::log;
        use syn::parse_quote;

        use mini_ir::{Context, kind_of, type_of};

        use crate::item::MiniImpl;
        use crate::mini_item::MiniItem;
        use crate::stmt::MiniBlock;
        use crate::ToMiniIrTerm;

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

            let converted_type = type_of(&context, converted.clone()).unwrap();
            println!("Type: {}", &converted_type);

            let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
            println!("Kind: {}", &converted_kind);

            // Assert
            //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
        }

        #[test]
        fn simple_impl_no_self() {
            // Arrange
            let mini: MiniImpl = parse_quote!(
                impl Test {
                    fn hello(arg1: bool) {}
                }
            );

            let context = Context::new();

            println!("\n{:#?}", &mini);

            // Act
            let converted = mini.convert_term();
            println!("Lambda: {}", &converted);

            let converted_type = type_of(&context, converted.clone()).unwrap();
            println!("Type: {}", &converted_type);

            let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
            println!("Kind: {}", &converted_kind);

            // Assert
            //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
        }

        #[test]
        fn simple_impl_no_self_generic() {
            // Arrange
            let mini: MiniImpl = parse_quote!(
                impl<T> Test {
                    fn hello(arg1: T) {}
                }
            );

            let context = Context::new();

            println!("\n{:#?}", &mini);

            // Act
            let converted = mini.convert_term();
            println!("Lambda: {}", &converted);

            let converted_type = type_of(&context, converted.clone()).unwrap();
            println!("Type: {}", &converted_type);

            let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
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

            let converted_type = type_of(&context, converted.clone()).unwrap();
            println!("Type: {}", &converted_type);

            let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
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

            let converted_type = type_of(&context, converted.clone()).unwrap();
            println!("Type: {}", &converted_type);

            let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
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

            let converted_type = type_of(&context, converted.clone()).unwrap();
            println!("Type: {}", &converted_type);

            let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
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

            let converted_type = type_of(&context, converted.clone()).unwrap();
            println!("Type: {}", &converted_type);

            let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
            println!("Kind: {}", &converted_kind);

            // Assert
            //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
        }
    }

    mod traits {
        use paris::log;
        use syn::parse_quote;
        use mini_ir::{Context, type_of};
        use crate::mini_item::MiniItem;
        use crate::ToMiniIrTerm;
        use mini_ir::Type as IRType;
        use mini_ir::BaseType;
        use crate::mini_file::MiniFile;

        #[test]
        fn impl_clone() {
            // Arrange
            let mini: MiniFile = parse_quote!(
                trait Clone {
                    fn clone(&self) -> Self;
                }

                impl Clone for bool {
                    // Todo: This should actually panic because the type is not the same as specified in the class
                    fn clone(self) -> Self {
                        self
                    }
                }

                fn main() -> bool {
                    Clone::clone(&true)
                }
            );
            let context = Context::new();

            log!("<blue>======== AST =======</>");
            println!("{:#?}", &mini);

            // Act
            let converted = mini.convert_term();
            log!("<blue>====== Lambda ======</>");
            println!("{}", &converted);
            log!("<blue>==== Type-Check ====</>");

            let converted_type = type_of(&context, converted.clone());
            log!("<blue>======= Type =======</>");
            println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

            log!("<blue>====================</>\n");
            // Assert
            assert!(matches!(converted_type, Ok(..)))
        }

        #[test]
        fn impl_clone_generic_vec() {
            // Arrange
            let mini: MiniFile = parse_quote!(
                trait Clone {
                    fn clone(&self) -> Self;
                }

                struct Vec<T> {
                    t: T,
                }

                impl Clone for bool {
                    // Todo: This should actually panic because the type is not the same as specified in the class
                    fn clone(self) -> Self {
                        self
                    }
                }

                impl Clone for Vec<T> where T: Clone {
                    fn clone(self) -> Self {
                        self
                    }
                }

                fn main() -> Vec<bool> {
                    let v = Vec::<bool>{t: true};
                    Clone::clone(&v)
                }
            );
            let context = Context::new();

            log!("<blue>======== AST =======</>");
            println!("{:#?}", &mini);

            // Act
            let converted = mini.convert_term();
            log!("<blue>====== Lambda ======</>");
            println!("{}", &converted);
            log!("<blue>==== Type-Check ====</>");

            let converted_type = type_of(&context, converted.clone());
            log!("<blue>======= Type =======</>");
            println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

            log!("<blue>====================</>\n");
            // Assert
            assert!(matches!(converted_type, Ok(..)))
        }

        #[test]
        fn impl_clone_fail() {
            // Arrange
            let mini: MiniFile = parse_quote!(
                trait Clone {
                    fn clone(&self) -> Self;
                }

                impl Clone for i64 {
                    // Todo: This should actually panic because the type is not the same as specified in the class
                    fn clone(self) -> Self {
                        self
                    }
                }

                fn main() -> bool {
                    Clone::clone(&true)
                }
            );
            let context = Context::new();

            log!("<blue>======== AST =======</>");
            println!("{:#?}", &mini);

            // Act
            let converted = mini.convert_term();
            log!("<blue>====== Lambda ======</>");
            println!("{}", &converted);
            log!("<blue>==== Type-Check ====</>");

            let converted_type = type_of(&context, converted.clone());
            log!("<blue>======= Type =======</>");
            println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

            log!("<blue>====================</>\n");
            // Assert
            assert!(matches!(converted_type, Err(..)))
        }

        #[test]
        fn impl_trait_empty() {
            // Arrange
            let mini: MiniFile = parse_quote!(
                trait Test {

                }

                impl Test for bool {

                }
            );
            let context = Context::new();

            log!("<blue>======== AST =======</>");
            println!("{:#?}", &mini);

            // Act
            let converted = mini.convert_term();
            log!("<blue>====== Lambda ======</>");
            println!("{}", &converted);
            log!("<blue>==== Type-Check ====</>");

            let converted_type = type_of(&context, converted.clone());
            log!("<blue>======= Type =======</>");
            println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

            log!("<blue>====================</>\n");
            // Assert
            assert!(matches!(converted_type, Ok(..)))
        }


        #[test]
        fn impl_partial_eq() {
            // Arrange
            let mini: MiniFile = parse_quote!(
                trait PartialEq<T> {
                    fn eq(&self, other: &T) -> bool;
                }

                impl PartialEq<bool> for bool {
                    fn eq(&self, other: &bool) -> bool {
                        false
                    }
                }

                fn main() -> bool {
                    &true == &false
                }
            );
            let context = Context::new();

            log!("<blue>======== AST =======</>");
            println!("{:#?}", &mini);

            // Act
            let converted = mini.convert_term();
            log!("<blue>====== Lambda ======</>");
            println!("{}", &converted);
            log!("<blue>==== Type-Check ====</>");

            let converted_type = type_of(&context, converted.clone());
            log!("<blue>======= Type =======</>");
            println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

            log!("<blue>====================</>\n");
            // Assert
            assert!(matches!(converted_type, Ok(..)))
        }
    }
}