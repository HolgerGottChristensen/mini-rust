use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use quote::ToTokens;
use syn::{braced, parenthesized, Token, token, Type, WhereClause};
use syn::parse::{Parse, Parser, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::{Brace, Comma, Enum, Paren};

use mini_ir::{BaseType, Kind, Term};
use mini_ir::Type as FType;

use crate::{MiniGenerics, MiniIdent, MiniType, ToMiniIrKind, ToMiniIrTerm, ToMiniIrType};
use crate::mini_generics::MiniWhere;

#[derive(PartialEq, Clone)]
pub struct MiniEnum {
    pub enum_token: Enum,
    pub ident: MiniIdent,
    pub generics: MiniGenerics,
    pub brace: Brace,
    pub fields: Punctuated<MiniEnumVariant, Comma>,
}

impl Debug for MiniEnum {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(stringify!(MiniEnum))
            .field("ident", &self.ident)
            .field("generics", &self.generics)
            .field("fields", &self.fields.iter().collect::<Vec<_>>())
            .finish()
    }
}

impl Parse for MiniEnum {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let enum_token = Enum::parse(input)?;
        let ident = MiniIdent::parse(input)?;
        let generics = input.parse::<MiniGenerics>()?;
        let (where_clause, brace_token, variants) =
            data_enum(input)?;

        Ok(MiniEnum {
            enum_token,
            ident,
            generics: MiniGenerics {
                where_clause,
                ..generics
            },
            brace: brace_token,
            fields: variants,
        })
    }
}

pub fn data_enum(input: ParseStream) -> syn::Result<(Option<MiniWhere>, token::Brace, Punctuated<MiniEnumVariant, Token![,]>)> {
    let where_clause = input.parse().ok();

    let content;
    let brace = braced!(content in input);
    let variants = content.parse_terminated(MiniEnumVariant::parse)?;

    Ok((where_clause, brace, variants))
}

#[derive(PartialEq, Clone)]
pub struct MiniEnumVariant {
    pub ident: MiniIdent,
    pub paren: Option<Paren>,
    pub items: Option<Punctuated<Type, Comma>>,
}

impl Debug for MiniEnumVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct(stringify!(MiniEnumVariant));

        s.field("ident", &self.ident);

        if let Some(items) = &self.items {
            s.field("items", &items.iter().map(|a| MiniType(a.clone())).collect::<Vec<_>>());
        }

        s.finish()
    }
}

impl Parse for MiniEnumVariant {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = MiniIdent::parse(input)?;

        let (paren, items) = if input.peek(Paren) {
            let content;
            let paren = parenthesized!(content in input);
            let punctuated = content.parse_terminated(Type::parse)?;

            (Some(paren), Some(punctuated))
        } else {
            (None, None)
        };

        Ok(MiniEnumVariant {
            ident,
            paren,
            items,
        })
    }
}

// Todo: Add a field or somethings, that uniquely identifies an enum
impl ToMiniIrTerm for MiniEnum {
    fn convert_term(&self) -> Term {
        let variants = self.convert_type();
        let mut body = Term::Replacement;

        for field in &self.fields {
            match &field.items {
                // Construct a function that takes zero term arguments, but type arguments, with the name of the enum case.
                None => {
                    let mut variants = variants.clone();

                    for generic in self.generics.params.iter() {
                        variants = FType::TypeApp(Box::new(variants), Box::new(FType::TypeVar(generic.ident.to_string())))
                    }

                    let mut function = Term::Tagging(field.ident.0.to_string(), Box::new(Term::Unit), variants);

                    for generic in self.generics.params.iter().rev() {
                        function = Term::TermTypeAbs(generic.ident.to_string(), generic.ident.convert_kind(), Box::new(function));
                    }

                    body = Term::Let(
                        format!("{}::{}", self.ident.0.to_string(), field.ident.0.to_string()),
                        Box::new(function),
                        Box::new(body),
                    );
                }
                Some(fields) => {
                    let mut variants = variants.clone();

                    for generic in self.generics.params.iter() {
                        variants = FType::TypeApp(Box::new(variants), Box::new(FType::TypeVar(generic.ident.to_string())))
                    }

                    let mut tups = vec![];

                    for (index, ty) in fields.iter().enumerate() {
                        tups.push(Term::TermVar(format!("arg{}", index)));
                    }

                    let tup = if tups.len() == 1 {
                        tups.first().unwrap().clone()
                    } else {
                        Term::Tuple(tups)
                    };

                    let mut function = Term::Tagging(field.ident.0.to_string(), Box::new(tup), variants);

                    for (index, ty) in fields.iter().enumerate().rev() {
                        function = Term::TermAbs(format!("arg{}", index), MiniType(ty.clone()).convert_type(), Box::new(function))
                    }

                    function = Term::TermAbs("#default".to_string(), FType::Base(BaseType::Unit), Box::new(function));

                    for generic in self.generics.params.iter().rev() {
                        function = Term::TermTypeAbs(generic.ident.to_string(), generic.ident.convert_kind(), Box::new(function));
                    }

                    body = Term::Let(
                        format!("{}::{}", self.ident.0.to_string(), field.ident.0.to_string()),
                        Box::new(function),
                        Box::new(body),
                    );
                }
            }
        }

        body = Term::Define(self.ident.0.to_string(), self.convert_type(), Box::new(body));

        body
    }
}

impl ToMiniIrType for MiniEnum {
    fn convert_type(&self) -> FType {
        let map = self.fields.iter().map(|variant| {
            let case = match &variant.items {
                // If there are no items, we default to a unit type
                None => FType::Base(BaseType::Unit),
                Some(items) => {
                    if items.len() == 1 {
                        MiniType(items.first().unwrap().clone()).convert_type()
                    } else {
                        FType::Tuple(items.iter().map(|a| MiniType(a.clone()).convert_type()).collect::<Vec<_>>())
                    }
                }
            };
            (variant.ident.0.to_string(), case)
        });

        let hash = HashMap::from_iter(map);
        let mut body = FType::Variants(hash);

        // Todo: How will we handle generic bounds?
        // Add generics as TypeAbs
        for generic in self.generics.params.iter().rev() {
            body = FType::TypeAbs(generic.ident.to_string(), generic.ident.convert_kind(), Box::new(body));
        }

        body
    }
}

mod tests {
    use syn::parse_quote;

    use mini_ir::{BaseType, Binding, Context, kind_of, Substitutions, type_of};

    use crate::{MiniEnum, ToMiniIrTerm, ToMiniIrType};
    use crate::stmt::MiniBlock;

    #[test]
    fn parse_single_unit_case_enum() {
        // Arrange
        let mini: MiniEnum = parse_quote!(
            enum Test {
                Case1
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_type();
        let converted_kind = kind_of(&Context::new(), converted.clone()).unwrap();;

        println!("Type: {}", &converted);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_single_unit_case_enum_with_construction() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                enum Test {
                    Case1
                }

                Test::Case1
            }

        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_single_non_unit_case_enum_with_construction() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                enum Test {
                    Case1(i64)
                }

                Test::Case1(2)
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_single_non_unit_case_enum_with_construction2() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                enum Test {
                    Case1(i64, bool)
                }

                Test::Case1(2, true)
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_multi_unit_case_enum() {
        // Arrange
        let mini: MiniEnum = parse_quote!(
            enum Test {
                Case1,
                Case2
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_type();
        println!("Type: {}", &converted);

        let converted_kind = kind_of(&Context::new(), converted.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_multiple_single_param_case_enum() {
        // Arrange
        let mini: MiniEnum = parse_quote!(
            enum Test {
                Case1(i64),
                Case2(bool)
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_type();
        println!("Type: {}", &converted);

        let converted_kind = kind_of(&Context::new(), converted.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_multiple_multi_param_case_enum() {
        // Arrange
        let mini: MiniEnum = parse_quote!(
            enum Test {
                Case1(i64, i64),
                Case2(bool, i64, f64, f64)
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_type();
        println!("Type: {}", &converted);

        let converted_kind = kind_of(&Context::new(), converted.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_generic_enum() {
        // Arrange
        let mini: MiniEnum = parse_quote!(
            enum Test<T> {
                Case1(T),
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_type();
        println!("Type: {}", &converted);

        let converted_kind = kind_of(&Context::new(), converted.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_option_enum() {
        // Arrange
        let mini: MiniEnum = parse_quote!(
            enum Option<T> {
                Some(T),
                None,
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_type();
        println!("Type: {}", &converted);

        let converted_kind = kind_of(&Context::new(), converted.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_option_enum_construction_no_generics() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                enum Option {
                    Some(i64),
                    None
                }

                let x = Option::None;
                let y = Option::Some(3);

                y
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_option_enum_construction_none1() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                enum Option<T> {
                    Some(T),
                    None
                }

                let x = Option::None;

                x
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_option_enum_construction_none2() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                enum Option<T> {
                    Some(T),
                    None
                }

                let x = Option::None::<i64>;

                x
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_option_enum_construction_some() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                enum Option<T> {
                    Some(T),
                    None
                }

                let x = Option::Some::<i64>(3);

                x
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_generic_enum_construction_simple() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                enum Option<T> {
                    None
                }

                let x = Option::None::<i64>;

                x
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_result_enum() {
        // Arrange
        let mini: MiniEnum = parse_quote!(
            enum Result<T, E> {
                Ok(T),
                Err(E),
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_result_enum_construction() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                enum Result<T, E> {
                    Ok(T),
                    Err(E),
                }

                let ok = Result::Ok::<bool, i64>(true);
                let err = Result::Err::<bool, i64>(32);

                ok
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_result_enum_construction_with_nested_generic() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                enum Result<T, E> {
                    Ok(T),
                    Err(E),
                }

                fn test<T>(t: T) -> Result<T, bool> {
                    Result::Ok::<T, bool>(t)
                }

                let x = test::<i64>(3);
                let y = test::<bool>(true);

                x
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type); // Todo: Why does this type not get simplified?

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }
}