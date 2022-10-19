use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::iter::once;
use chalk_integration::interner::ChalkIr;
use chalk_integration::{Identifier, RawId, TypeKind, TypeSort};
use chalk_ir::{AdtId, Binders, Scalar, Ty, TyKind, VariableKinds};
use chalk_ir::interner::Interner;
use chalk_solve::clauses::builder::ClauseBuilder;
use chalk_solve::rust_ir;
use chalk_solve::rust_ir::{AdtDatum, AdtDatumBound, AdtFlags, AdtKind, AdtVariantDatum, Anonymize};
use proc_macro2::Ident;
use syn::punctuated::Punctuated;
use syn::{braced, Field, Token, Type, ItemStruct, WhereClause};
use syn::parse::{Parse, Parser, ParseStream};
use syn::token::{Brace, Colon, Comma, Struct};
use crate::{MiniEnum, MiniGenerics, MiniIdent, MiniType, ToChalkRustIR, ToSystemFOmegaTerm, ToSystemFOmegaType};
use system_f_omega::{BaseType, Kind, Term, Type as FType};
use crate::lowering::{Env, LowerParameterMap, LowerResult, LowerTypeKind, LowerWithEnv, VariableKind};

#[derive(PartialEq, Clone)]
pub struct MiniStruct {
    pub struct_token: Struct,
    pub ident: MiniIdent,
    pub generics: MiniGenerics,
    pub brace: Brace,
    pub fields: Punctuated<MiniStructField, Comma>
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
            fields
        })
    }
}

pub fn data_struct(
    input: ParseStream,
) -> syn::Result<(Option<WhereClause>, Brace, Punctuated<MiniStructField, Comma>)> {
    let mut lookahead = input.lookahead1();
    let mut where_clause = None;
    if lookahead.peek(Token![where]) {
        where_clause = Some(input.parse()?);
        lookahead = input.lookahead1();
    }

    let content;

    let brace =  braced!(content in input);
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

/*impl MiniStructField {
    fn convert_to_chalk(&self, env: &Env) -> Ty<ChalkIr> {
        Ty::new(ChalkIr, TyKind::Scalar(Scalar::Bool))
    }
}*/

impl ToSystemFOmegaTerm for MiniStruct {
    fn convert_term(&self) -> Term {
        Term::Define(self.ident.0.to_string(), self.convert_type(), Box::new(Term::Unit))
    }
}

// Todo: Add a field that uniquely identifies a struct
impl ToSystemFOmegaType for MiniStruct {
    fn convert_type(&self) -> FType {
        let map = self.fields.iter().map(|field| {
            let case = MiniType(field.ty.clone()).convert_type();
            (field.ident.0.to_string(), case)
        });

        let map = map.chain(once((format!("#{}", self.ident.0.to_string()), FType::Base(BaseType::Unit))));

        let hash = HashMap::from_iter(map);
        let mut body = FType::Record(hash);

        // Todo: How will we handle generic bounds?
        // Add generics as TypeAbs
        for generic in self.generics.params.iter().rev() {
            body = FType::TypeAbs(generic.ident.0.to_string(), Kind::KindStar, Box::new(body));
        }

        body
    }
}
// Todo: impl trait, write test with simple Struct, Trait & Impl

impl LowerWithEnv for (&MiniStruct, AdtId<ChalkIr>) {
    type Lowered = AdtDatum<ChalkIr>;

    fn lower(&self, env: &Env) -> LowerResult<Self::Lowered> {
        let (mini_struct, adt_id) = self;

        let binders = env.in_binders(mini_struct.all_parameters(), |env| {
            Ok(AdtDatumBound {
                variants: vec![AdtVariantDatum {
                    fields: mini_struct.fields.iter().map(|f| Ty::new(ChalkIr, TyKind::Scalar(Scalar::Bool))).collect()
                }],
                /*variants: mini_struct
                    .variants
                    .iter()
                    .map(|v| {
                        let fields: LowerResult<_> =
                            v.fields.iter().map(|f| f.ty.lower(env)).collect();
                        Ok(AdtVariantDatum { fields: fields? })
                    })
                    .collect::<LowerResult<_>>()?,*/
                where_clauses: vec![], //adt_defn.where_clauses.lower(env)?,
            })
        })?;

        Ok(AdtDatum {
            binders,
            id: *adt_id,
            flags: AdtFlags {
                upstream: false,
                fundamental: false,
                phantom_data: false
            },
            kind: AdtKind::Struct
        })
    }
}

impl LowerTypeKind for MiniStruct {
    fn lower_type_kind(&self) -> LowerResult<TypeKind> {
        Ok(TypeKind {
            sort: TypeSort::Adt,
            name: Identifier::from(self.ident.to_string()),
            binders: Binders::new(
                VariableKinds::from_iter(ChalkIr, self.all_parameters().anonymize()),
                chalk_integration::Unit,
            ),
        })

    }
}

impl LowerParameterMap for MiniStruct {
    fn synthetic_parameters(&self) -> Option<chalk_ir::WithKind<ChalkIr, Identifier>> {
        None
    }
    fn declared_parameters(&self) -> Vec<VariableKind> {
        self.generics.params.iter().map(|a| VariableKind(Identifier::from(a.ident.to_string()))).collect()
        //&self.variable_kinds
    }
}


mod tests {
    use std::collections::HashMap;
    use quote::quote;
    use syn::parse_quote;
    use system_f_omega::{add_binding, BaseType, Binding, Context, kind_of, Term, Type, type_of};
    use crate::{MiniEnum, MiniExprReference, MiniFn, MiniLitExpr, MiniStmt, MiniStruct, ToSystemFOmegaTerm, ToSystemFOmegaType};
    use crate::mini_expr::MiniExpr;

    mod chalk {
        use std::collections::BTreeMap;
        use chalk_integration::db::ChalkDatabase;
        use chalk_integration::lowering::Lower;
        use chalk_integration::program::Program;
        use chalk_integration::query::LoweringDatabase;
        use chalk_integration::SolverChoice;
        use chalk_solve::logging_db::LoggingRustIrDatabase;
        use syn::parse_quote;
        use crate::{MiniStruct, ToChalkRustIR};
        use crate::lowering::environment;
        use crate::mini_file::MiniFile;

        #[test]
        fn convert_to_chalk_rust() {
            // Arrange
            let mini: MiniFile = parse_quote!(
                struct Test {}
            );

            println!("\n{:#?}", &mini);

            // Act
            let x = mini.lower().unwrap();
            println!("\n{:#?}", &x);
            println!("\n{:#?}", environment(x));

            // Assert
        }

        #[test]
        fn convert_to_chalk_rust_with_fields() {
            // Arrange
            let mini: MiniFile = parse_quote!(
                struct Test {
                    x: i64,
                    y: f64,
                }
            );

            println!("\n{:#?}", &mini);

            // Act
            let x = mini.lower().unwrap();
            println!("\n{:#?}", &x);
            println!("\n{:#?}", environment(x));

            // Assert
        }

        #[test]
        fn convert_to_chalk_rust_generic() {
            // Arrange
            let mini: MiniFile = parse_quote!(
                struct Test<T> {}
            );

            println!("\n{:#?}", &mini);

            // Act
            let x = mini.lower().unwrap();
            println!("\n{:#?}", &x);
            println!("\n{:#?}", environment(x));

            // Assert
        }
    }

    #[test]
    fn parse_zero_field() {
        // Arrange
        let mini: MiniStruct = parse_quote!(
            struct Test {}
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
        let converted_kind = kind_of(&Context::new(), converted.clone());

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
        let converted_kind = kind_of(&Context::new(), converted.clone());

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
        let converted_kind = kind_of(&Context::new(), converted.clone());

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
        let converted_kind = kind_of(&Context::new(), converted.clone());

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
                struct Test {
                    test: i64
                }

                struct Test2 {
                    test: i64
                }

                // This should not typecheck because we expect the return type to be of type Test2
                // but it is returning a type of Test.
                // If it fails, we correctly differentiate between two structs with the same fields and same types.
                fn t(x: Test) -> Test2 {
                    x
                }
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone());
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
        let converted_type = type_of(&context, converted.clone());
        //let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        //println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }
}