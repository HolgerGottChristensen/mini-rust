use std::fmt::{Debug, Formatter};

use paris::formatter::colorize_string;
use proc_macro2::Span;
use syn::{Error, parenthesized, token, Token, Type};
use syn::parse::{Parse, ParseStream};
use syn::parse::discouraged::Speculative;
use syn::punctuated::Punctuated;
use syn::token::{And, Colon, Comma, Mut, Paren, RArrow, SelfValue};

use mini_ir::{BaseType, Context, Kind, Substitutions, Term, type_of};
use mini_ir::Type as FType;
use mini_ir::Type::TypeVar;

use crate::{MiniGenerics, MiniIdent, MiniType, ToMiniIrKind, ToMiniIrTerm, ToMiniIrType};
use crate::stmt::MiniBlock;

#[derive(PartialEq, Clone)]
pub struct MiniFn {
    pub fn_token: token::Fn,
    pub ident: MiniIdent,
    pub generics: MiniGenerics,
    pub paren_token: Paren,
    pub inputs: Punctuated<MiniFnArg, Comma>,
    pub return_type: Option<(RArrow, Box<MiniType>)>,
    pub block: Box<MiniBlock>,
}

#[derive(PartialEq, Clone)]
pub enum MiniFnArg {
    Receiver {
        reference: Option<And>,
        mutability: Option<Mut>,
        self_token: SelfValue,
    },
    Typed {
        pat: MiniIdent,
        colon_token: Colon,
        ty: Box<Type>,
    },
}

impl MiniFn {
    fn parse_fn_args(input: ParseStream) -> syn::Result<Punctuated<MiniFnArg, Token![,]>> {
        let mut args = Punctuated::new();
        let mut has_receiver = false;

        while !input.is_empty() {
            let mut arg: MiniFnArg = input.parse()?;
            match &mut arg {
                MiniFnArg::Receiver { .. } if has_receiver => {
                    return Err(Error::new(
                        Span::call_site(),
                        "unexpected second method receiver",
                    ));
                }
                MiniFnArg::Receiver { .. } if !args.is_empty() => {
                    return Err(Error::new(
                        Span::call_site(),
                        "unexpected method receiver",
                    ));
                }
                MiniFnArg::Receiver { .. } => {
                    has_receiver = true;
                }
                MiniFnArg::Typed { .. } => (),
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

impl Debug for MiniFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("MiniFn");

        s.field("ident", &self.ident);
        s.field("generics", &self.generics);
        s.field("inputs", &self.inputs.iter().collect::<Vec<_>>());

        s.field("return", &self.return_type.clone());

        s.field("block", &self.block);

        s.finish()
    }
}

impl Parse for MiniFn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fn_token: Token![fn] = input.parse()?;
        let ident: MiniIdent = input.parse()?;
        let mut generics: MiniGenerics = input.parse()?;

        let content;
        let paren_token = parenthesized!(content in input);
        let mut inputs = MiniFn::parse_fn_args(&content)?;

        let output_ty = if input.peek(RArrow) {
            Some((RArrow::parse(input)?, Box::new(MiniType::parse(input)?)))
        } else {
            None
        };

        generics.where_clause = input.parse()?;

        Ok(MiniFn {
            fn_token,
            ident,
            generics,
            paren_token,
            inputs,
            return_type: output_ty,
            block: Box::new(input.parse()?),
        })
    }
}

impl Debug for MiniFnArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MiniFnArg::Receiver { reference, mutability, self_token } => {
                let reference = reference.map(|a| "&").unwrap_or("");
                let mutability = mutability.map(|a| "mut").unwrap_or("");

                let s = format!("<yellow><b>{}{}self</>", reference, mutability);

                write!(f, "{}", colorize_string(s))
            }
            MiniFnArg::Typed { pat, colon_token, ty } => {
                f.debug_struct("Typed")
                    .field("pat", &pat)
                    .field("ty", &MiniType(*ty.clone()))
                    .finish()
            }
        }
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

        fn parse_typed(input: ParseStream) -> syn::Result<(MiniIdent, Colon, Type)> {
            let pat = MiniIdent::parse(input)?;//multi_pat(input)?;
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
                    self_token,
                });
            }
        }

        let (pat, colon, ty) = input.call(parse_typed)?;
        Ok(MiniFnArg::Typed {
            pat,
            colon_token: colon,
            ty: Box::new(ty),
        })
    }
}

impl ToMiniIrType for MiniFn {
    fn convert_type(&self) -> FType {
        type_of(&Context::new(), ToMiniIrTerm::convert_term(self), &mut Substitutions::new()).unwrap()
    }
}

impl ToMiniIrTerm for MiniFn {
    fn convert_term(&self) -> Term {
        let mut body = self.block.convert_term();

        let return_type = match &self.return_type {
            None => {
                FType::Base(BaseType::Unit)
            }
            Some((_, return_type)) => {
                return_type.convert_type()
            }
        };

        // Todo: Add a define for all returns that uses a unique symbol
        // Ascribe with the return type
        body = Term::Ascribe(
            Box::new(body),
            TypeVar("#Return".to_string()),
        );

        body = Term::Define("#Return".to_string(), return_type, Box::new(body));


        for param in self.inputs.iter().rev() {
            match param {
                MiniFnArg::Receiver { reference, mutability, self_token } => {

                    // Todo: How do we handle mutability of the receiver?
                    let mut ty = FType::TypeVar("#Self".to_string());
                    if reference.is_some() {
                        ty = FType::Reference(Box::new(ty));
                    }

                    body = Term::TermAbs(
                        "self".to_string(),
                        ty,
                        Box::new(body),
                    );
                }
                MiniFnArg::Typed { pat, colon_token, ty } => {
                    body = Term::TermAbs(
                        pat.0.to_string(),
                        MiniType(*ty.clone()).convert_type(),
                        Box::new(body),
                    );
                }
            }
        }

        // By default we add a one parameter abstraction, to make functions with 0 arguments to valid abstractions.
        body = Term::TermAbs(
            "arg0".to_string(),
            FType::Base(BaseType::Unit),
            Box::new(body),
        );

        // Todo: How will we handle generic bounds?
        // Add generics as TypeAbs
        for generic in self.generics.params.iter().rev() {
            body = Term::TermTypeAbs(generic.ident.to_string(), generic.ident.convert_kind(), Box::new(body));
        }

        body
    }
}

mod tests {
    use syn::parse_quote;

    use mini_ir::{Context, Substitutions, type_of};

    use crate::{MiniFn, ToMiniIrTerm};
    use crate::stmt::MiniBlock;

    #[test]
    fn parse_fn_simple_0_arg() {
        // Arrange
        let mini: MiniFn = parse_quote!(
            fn hello() {}
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_simple_0_arg_with_return() {
        // Arrange
        let mini: MiniFn = parse_quote!(
            fn hello() -> i64 {
                0
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_simple_0_arg_with_explicit_return() {
        // Arrange
        let mini: MiniFn = parse_quote!(
            fn hello() -> i64 {
                return 0
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_simple_0_arg_with_multiple_explicit_return() {
        // Arrange
        let mini: MiniFn = parse_quote!(
            fn hello() -> i64 {
                return 2;
                let x = 43;
                x
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_nested_simple() {
        // Arrange
        let mini: MiniFn = parse_quote!(
            fn hello() -> bool {

                fn test() -> bool {
                    true
                }

                test()
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_simple_1_arg() {
        // Arrange
        let mini: MiniFn = parse_quote!(
             fn hello(arg1: i64) {}
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_simple_multiple_arg() {
        // Arrange
        let mini: MiniFn = parse_quote!(
             fn hello(arg1: i64, arg2: i64, arg3: i64) {}
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_simple_1_arg_generic() {
        // Arrange
        let mini: MiniFn = parse_quote!(
             fn hello<T>(arg1: T) {}
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_HKT() {
        // Arrange
        let mini: MiniFn = parse_quote!(
             fn hello<T<U>>() {}
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_HKT_nested() {
        // Arrange
        let mini: MiniFn = parse_quote!(
             fn hello<T<U<I>>>() {}
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_HKT_multiple() {
        // Arrange
        let mini: MiniFn = parse_quote!(
             fn hello<T<U, I>>() {}
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_simple_1_arg_generic_applied() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
             {
                 fn hello<T>(arg1: T) -> T {
                    arg1
                 }

                 hello::<i64>(2)
             }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_simple_2_arg_generic_multiple() {
        // Arrange
        let mini: MiniFn = parse_quote!(
            fn hello<T, U>(arg1: T, arg2: U) {}
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_higher_order_function() {
        // Arrange
        let mini: MiniFn = parse_quote!(
             fn hello(arg1: fn(i64) -> i64) {}
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_higher_order_function_with_generics() {
        // Arrange
        let mini: MiniFn = parse_quote!(
             fn hello<T>(arg1: fn(T) -> T) {}
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_with_body() {
        // Arrange
        let mini: MiniFn = parse_quote!(
            fn hello() -> i64 {
                let x = 42;
                let y = false;
                x
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_fn_return_first_param() {
        // Arrange
        let mini: MiniFn = parse_quote!(
            fn hello(x: i64) -> i64 {
                x
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();

        println!("\nLambda:\n{}", &converted);
        println!("\nType:\n{}", type_of(&Context::new(), converted, &mut Substitutions::new()).unwrap());

        // Assert
        //assert!(matches!(actual, CarbideExpr::Lit(LitExpr {lit: Lit::Int(_)})))
    }

    #[test]
    fn parse_higher_order_function_applied() {
        // Arrange
        let mini: MiniFn = parse_quote!(
             fn hello(arg1: fn(i64) -> i64) -> i64 {
                arg1(3)
            }
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