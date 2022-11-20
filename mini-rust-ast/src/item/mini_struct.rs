use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::iter::once;

use syn::{braced, Token, Type, WhereClause};
use syn::parse::{Parse, Parser, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::{Brace, Colon, Comma, Struct};

use mini_ir::{BaseType, Kind, Term, Type as FType};

use crate::{MiniEnum, MiniGenerics, MiniIdent, MiniType, ToMiniIrKind, ToMiniIrTerm, ToMiniIrType};
use crate::mini_generics::MiniWhere;

#[derive(PartialEq, Clone)]
pub struct MiniStruct {
    pub struct_token: Struct,
    pub ident: MiniIdent,
    pub generics: MiniGenerics,
    pub brace: Brace,
    pub fields: Punctuated<MiniStructField, Comma>,
}

impl Debug for MiniStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MiniStruct")
            .field("ident", &self.ident)
            .field("generics", &self.generics)
            .field("fields", &self.fields.iter().collect::<Vec<_>>())
            .finish()
    }
}

impl Parse for MiniStruct {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let struct_token = input.parse::<Token![struct]>()?;
        let ident = MiniIdent::parse(input)?;
        let generics = MiniGenerics::parse(input)?;
        let (where_clause, brace, fields) = data_struct(input)?;


        Ok(MiniStruct {
            struct_token,
            ident,
            generics: MiniGenerics {
                where_clause,
                ..generics
            },
            brace,
            fields,
        })
    }
}

pub fn data_struct(
    input: ParseStream,
) -> syn::Result<(Option<MiniWhere>, Brace, Punctuated<MiniStructField, Comma>)> {
    let mut lookahead = input.lookahead1();
    let mut where_clause = None;
    if lookahead.peek(Token![where]) {
        where_clause = Some(input.parse()?);
        lookahead = input.lookahead1();
    }

    let content;

    let brace = braced!(content in input);
    let fields = content.parse_terminated(MiniStructField::parse)?;

    Ok((where_clause, brace, fields))
}

#[derive(PartialEq, Clone)]
pub struct MiniStructField {
    pub ident: MiniIdent,
    pub colon_token: Colon,
    pub ty: Type,
}

impl Debug for MiniStructField {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MiniStructField")
            .field("ident", &self.ident)
            .field("ty", &MiniType(self.ty.clone()))
            .finish()
    }
}

impl Parse for MiniStructField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(MiniStructField {
            ident: MiniIdent::parse(input)?,
            colon_token: Colon::parse(input)?,
            ty: Type::parse(input)?,
        })
    }
}


impl ToMiniIrTerm for MiniStruct {
    fn convert_term(&self) -> Term {
        Term::Define(self.ident.0.to_string(), self.convert_type(), Box::new(Term::Replacement))
    }
}

// Todo: Add a field that uniquely identifies a struct
impl ToMiniIrType for MiniStruct {
    fn convert_type(&self) -> FType {
        let map = self.fields.iter().map(|field| {
            let case = MiniType(field.ty.clone()).convert_type();
            (field.ident.0.to_string(), case)
        });

        let map = map.chain(once((format!("#{}", self.ident.0.to_string()), FType::Base(BaseType::Unit))));

        let hash = HashMap::from_iter(map);
        let mut body = FType::Record(hash);

        // Todo: How will we handle generic bounds?
        /*for generic in self.generics.params.iter().rev() {
            for bound in &generic.bounds {
                body = FType::Predicate("€".to_string(), Box::new(FType::TypeVar(
                    MiniPath(bound.path.clone()).as_ident() // Todo: what about generics in bounds?
                )), Box::new(body))
            }
        }*/
        // Add generics as TypeAbs
        for generic in self.generics.params.iter().rev() {
            body = FType::TypeAbs(generic.ident.to_string(), generic.ident.convert_kind(), Box::new(body));
        }

        body
    }
}


mod tests {
    use syn::parse_quote;

    use mini_ir::{BaseType, Binding, Context, kind_of, type_of};

    use crate::{MiniStruct, ToMiniIrTerm, ToMiniIrType};
    use crate::mini_expr::MiniExpr;

    #[test]
    fn parse_zero_field() {
        // Arrange
        let mini: MiniStruct = parse_quote!(
            struct Test {}
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
    fn parse_single_field() {
        // Arrange
        let mini: MiniStruct = parse_quote!(
            struct Test {
                field1: i64
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
    fn parse_multiple_fields() {
        // Arrange
        let mini: MiniStruct = parse_quote!(
            struct Test {
                field1: i64,
                field2: (bool, bool),
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
    fn parse_generic_field() {
        // Arrange
        let mini: MiniStruct = parse_quote!(
            struct Test<T> {
                field1: T
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
    fn parse_generic_field_with_bound() {
        // Arrange
        let mini: MiniStruct = parse_quote!(
            struct Test<T: Clone> {
                field1: T
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
    fn parse_multiple_generic_fields() {
        // Arrange
        let mini: MiniStruct = parse_quote!(
            struct Test<T, U> {
                field1: (T, U),
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
    #[should_panic] // See expr comments
    fn parse_uniquely_identify_structs_failing() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                trait Clone<Self> { // lambda Self. over ^Clone ∵ Clone [Self] in Unit
                    fn clone(s: &Self) -> Self ...
                }

                impl Clone<bool> for bool { // inst ^Clone ∵ Clone [bool] = ... in Unit
                    fn clone(s: &bool) -> bool ...
                }

                impl Clone<i64> for i64 {
                    fn clone(s: &i64) -> i64 ...
                }

                struct Test<T: Clone> { // λT::*. (^Clone ∵ Clone [T]).⟨test: T, #Test: Unit⟩
                    test: T
                }

                impl<T> Clone<Test> for Test<T> where T: Debug {
                    fn clone(s: &Test<T>) ...
                }

                fn main() {
                    let s = Test::<bool> { test: true } // (^Clone ∵ Clone [bool]).⟨test: bool, #Test: Unit⟩
                    result(s)
                }

                // The type contains all the methods that are brought in by bounds.
                // We add all of them to the environment at the start of the function.

                fn result(x: Test<bool>, y: Test<i64>) -> T {
                    Clone::<bool>::clone(&x.test)  // Clone::<Self>::clone
                    //Clone::clone(&y.test)  // Clone::<Self>::clone
                }


                fn main2() {
                    simple::<bool>(&true) // Clone T => &T -> T [bool] BoolCloneDict &true
                }

                // Add Clone T to context as an assumption
                fn simple<T: Clone>(x: &T, y: &bool) -> T { // lambda T. Clone T => &T -> T


                    Clone::<T>::clone(x) // Check if Clone T is a specialization of something in the context
                    Clone::<bool>::clone(y)
                }

            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone()).unwrap();
        //let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        //println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_uniquely_identify_structs() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                struct Test {
                    test: i64
                }

                struct Test2 {
                    test: i64
                }

                fn t(x: Test) -> Test {
                    x
                }
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone()).unwrap();
        //let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        //println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }
}