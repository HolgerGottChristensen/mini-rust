use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use paris::formatter::colorize_string;
use proc_macro2::Ident;
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::{braced, Field, Generics, parenthesized, Token, Type, ItemEnum, token, WhereClause};
use syn::parse::{Parse, Parser, ParseStream};
use syn::token::{Brace, Colon, Comma, Enum, Paren, Struct};
use system_f_omega::{BaseType, Context, Kind, Term, type_of};
use system_f_omega::Type as FType;
use crate::{MiniGenerics, MiniIdent, MiniType, ToSystemFOmegaTerm, ToSystemFOmegaType};

#[derive(PartialEq, Clone)]
pub struct MiniEnum {
    pub enum_token: Enum,
    pub ident: MiniIdent,
    pub generics: MiniGenerics,
    pub brace: Brace,
    pub fields: Punctuated<MiniEnumVariant, Comma>
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
            fields: variants
        })
    }
}

pub fn data_enum(input: ParseStream) -> syn::Result<(Option<WhereClause>, token::Brace, Punctuated<MiniEnumVariant, Token![,]>)> {
    let where_clause = input.parse()?;

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
impl ToSystemFOmegaTerm for MiniEnum {
    fn convert_term(&self) -> Term {
        Term::Define(self.ident.0.to_string(), self.convert_type(), Box::new(Term::Unit))
        // Todo: We probably also need to for all cases in the enum define a "let" function that takes the correct params and creates an enum case.
    }
}

impl ToSystemFOmegaType for MiniEnum {
    fn convert_type(&self) -> FType {
        let map = self.fields.iter().map(|variant| {
            let case = match &variant.items {
                // If there are no items, we default to a unit type
                None => FType::Base(BaseType::Unit),
                Some(items) => {
                    FType::Tuple(items.iter().map(|a| MiniType(a.clone()).convert_type()).collect::<Vec<_>>())
                }
            };
            (variant.ident.0.to_string(), case)
        });

        let hash = HashMap::from_iter(map);
        let mut body = FType::Variants(hash);

        // Todo: How will we handle generic bounds?
        // Add generics as TypeAbs
        for generic in self.generics.params.iter().rev() {
            body = FType::TypeAbs(generic.ident.0.to_string(), Kind::KindStar, Box::new(body));
        }

        body
    }
}

mod tests {
    use std::collections::HashMap;
    use quote::quote;
    use syn::parse_quote;
    use system_f_omega::{add_binding, BaseType, Binding, Context, kind_of, Term, Type, type_of};
    use crate::{MiniEnum, MiniExprReference, MiniFn, MiniLitExpr, MiniStmt, ToSystemFOmegaTerm, ToSystemFOmegaType};
    use crate::mini_expr::MiniExpr;
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
        let converted_kind = kind_of(&Context::new(), converted.clone());

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

        let converted_type = type_of(&Context::new(), converted.clone());
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone());
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

        let converted_kind = kind_of(&Context::new(), converted.clone());
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

        let converted_kind = kind_of(&Context::new(), converted.clone());
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

        let converted_kind = kind_of(&Context::new(), converted.clone());
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

        let converted_kind = kind_of(&Context::new(), converted.clone());
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

        let converted_kind = kind_of(&Context::new(), converted.clone());
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
        let converted = mini.convert_type();
        println!("Type: {}", &converted);

        let converted_kind = kind_of(&Context::new(), converted.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }
}