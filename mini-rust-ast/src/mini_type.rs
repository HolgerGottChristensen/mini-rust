use std::fmt::{Debug, Formatter};
use paris::formatter::colorize_string;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{Path, ReturnType, Type};
use mini_ir::{BaseType, Term};
use mini_ir::Type as FType;
use mini_ir::Type::TypeApp;
use crate::{ToSystemFOmegaTerm, ToSystemFOmegaType, TYPE_COLOR};
use crate::mini_path::MiniPath;

#[derive(PartialEq, Clone)]
pub struct MiniType(pub Type);

impl MiniType {
    pub fn path(&self) -> MiniPath {
        MiniPath(match &self.0 {
            Type::Path(p) => p.path.clone(),
            _ => panic!("Not a path type"),
        })
    }
}

impl Parse for MiniType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(MiniType(Type::parse(input)?))
    }
}

impl Debug for MiniType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = colorize_string(format!("{}{}</>", TYPE_COLOR, self.0.to_token_stream().to_string()));
        write!(f, "{}", string)
    }
}

impl ToSystemFOmegaType for MiniType {
    fn convert_type(&self) -> FType {
        match &self.0 {
            Type::BareFn(p) => {
                let mut t = match &p.output {
                    ReturnType::Default => {
                        FType::Base(BaseType::Unit)
                    },
                    ReturnType::Type(_, i) => {
                        MiniType(*i.clone()).convert_type()
                    }
                };

                for input in p.inputs.iter().rev() {
                    let n = MiniType(input.ty.clone());

                    t = FType::TypeArrow(
                        Box::new(n.convert_type()),
                        Box::new(t)
                    );
                }

                // Add a single unit parameter to make sure zero argument functions are still functions.
                t = FType::TypeArrow(
                    Box::new(FType::Base(BaseType::Unit)),
                    Box::new(t)
                );

                t
            },
            Type::Paren(p) => {
                let t = MiniType(*p.elem.clone());
                t.convert_type()
            },
            Type::Path(p) => {
                let t = MiniPath(p.path.clone()).as_ident();

                let mut res = match t.as_str() {
                    "i64" => FType::Base(BaseType::Int),
                    "f64" => FType::Base(BaseType::Float),
                    "bool" => FType::Base(BaseType::Bool),
                    "()" => FType::Base(BaseType::Unit),
                    t => {
                        FType::TypeVar(t.to_string())
                    } // Todo: We need to lookup the type of user defined structs and more.
                };

                for generic in MiniPath(p.path.clone()).generics() {
                    res = TypeApp(Box::new(res), Box::new(generic));
                }

                res
            },
            Type::Reference(r) => {
                // Todo: What if we have mutable references.
                let new = MiniType(*r.elem.clone());
                FType::Reference(Box::new(new.convert_type()))
            },
            Type::Slice(_) => todo!(),
            Type::Tuple(t) => {
                if t.elems.len() > 0{
                    let mut types = vec![];

                    for elem in &t.elems {
                        types.push(MiniType(elem.clone()).convert_type());
                    }

                    FType::Tuple(types)
                } else {
                    FType::Base(BaseType::Unit)
                }
            }
            _ => unimplemented!()
        }
    }
}

mod tests {
    use quote::quote;
    use syn::parse_quote;
    use mini_ir::{Context, kind_of, type_of, Type as FType, BaseType, Type};
    use crate::{MiniType, ToSystemFOmegaType};

    #[test]
    fn convert_unit() {
        // Arrange

        let mini: MiniType = parse_quote!(
            ()
        );
        println!("\nMini: {:#?}", &mini);

        // Act
        let converted = mini.convert_type();

        println!("Type: {}", &converted);
        println!("Kind: {}", kind_of(&Context::new(),converted.clone()));

        // Assert
        assert_eq!(converted, FType::Base(BaseType::Unit));
    }

    #[test]
    fn convert_f64() {
        // Arrange

        let mini: MiniType = parse_quote!(
            f64
        );
        println!("\nMini: {:#?}", &mini);

        // Act
        let converted = mini.convert_type();

        println!("Type: {}", &converted);
        println!("Kind: {}", kind_of(&Context::new(),converted.clone()));

        // Assert
        assert_eq!(converted, FType::Base(BaseType::Float));
    }

    #[test]
    fn convert_i64() {
        // Arrange

        let mini: MiniType = parse_quote!(
            i64
        );
        println!("\nMini: {:#?}", &mini);

        // Act
        let converted = mini.convert_type();

        println!("Type: {}", &converted);
        println!("Kind: {}", kind_of(&Context::new(),converted.clone()));

        // Assert
        assert_eq!(converted, FType::Base(BaseType::Int));
    }

    #[test]
    fn convert_bool() {
        // Arrange

        let mini: MiniType = parse_quote!(
            bool
        );
        println!("\nMini: {:#?}", &mini);

        // Act
        let converted = mini.convert_type();

        println!("Type: {}", &converted);
        println!("Kind: {}", kind_of(&Context::new(),converted.clone()));

        // Assert
        assert_eq!(converted, FType::Base(BaseType::Bool));
    }

    #[test]
    fn convert_reference() {
        // Arrange

        let mini: MiniType = parse_quote!(
            &bool
        );
        println!("\nMini: {:#?}", &mini);

        // Act
        let converted = mini.convert_type();

        println!("Type: {}", &converted);
        println!("Kind: {}", kind_of(&Context::new(),converted.clone()));

        // Assert
        assert_eq!(converted, FType::Reference(Box::new(FType::Base(BaseType::Bool))));
    }

    #[test]
    fn convert_generic() {
        // Todo: we need to take some kind of environment to look up the type of a generic parsed to a function, by the name of the generic?
        // Arrange
        let mini: MiniType = parse_quote!(
            T
        );
        println!("\nMini: {:#?}", &mini);

        // Act
        let converted = mini.convert_type();

        println!("Type: {}", &converted);
        //println!("Kind: {}", kind_of(&Context::new(),converted.clone()));

        // Assert
        //assert_eq!(converted, FType::Reference(Box::new(FType::Base(BaseType::Bool))));
        assert_eq!(Type::TypeVar("T".to_string()), converted);
    }

    #[test]
    fn convert_enum_and_struct() {
        // Todo: How do we handle types of enums and structs?
        // Arrange
        let mini: MiniType = parse_quote!(
            Option
        );
        println!("\nMini: {:#?}", &mini);

        // Act
        let converted = mini.convert_type();

        println!("Type: {}", &converted);
        //println!("Kind: {}", kind_of(&Context::new(),converted.clone()));

        // Assert
        //assert_eq!(converted, FType::Reference(Box::new(FType::Base(BaseType::Bool))));
        assert_eq!(Type::TypeVar("Option".to_string()), converted);
    }

    #[test]
    fn convert_tuple() {
        // Arrange

        let mini: MiniType = parse_quote!(
            (bool, i64, f64)
        );
        println!("\nMini: {:#?}", &mini);

        // Act
        let converted = mini.convert_type();

        println!("Type: {}", &converted);
        println!("Kind: {}", kind_of(&Context::new(),converted.clone()));

        // Assert
        assert_eq!(converted, FType::Tuple(vec![
            FType::Base(BaseType::Bool),
            FType::Base(BaseType::Int),
            FType::Base(BaseType::Float),
        ]));
    }

    #[test]
    fn convert_fn_no_return() {
        // Arrange

        let mini: MiniType = parse_quote!(
            fn()
        );
        println!("\nMini: {:#?}", &mini);

        // Act
        let converted = mini.convert_type();

        println!("Type: {}", &converted);
        println!("Kind: {}", kind_of(&Context::new(),converted.clone()));

        // Assert
        assert_eq!(converted, FType::TypeArrow(
            Box::new(FType::Base(BaseType::Unit)),
            Box::new(FType::Base(BaseType::Unit)),
        ));
    }

    #[test]
    fn convert_fn_1_arg_no_return() {
        // Arrange

        let mini: MiniType = parse_quote!(
            fn(i64)
        );
        println!("\nMini: {:#?}", &mini);

        // Act
        let converted = mini.convert_type();

        println!("Type: {}", &converted);
        println!("Kind: {}", kind_of(&Context::new(),converted.clone()));

        // Assert
        assert_eq!(converted, FType::TypeArrow(
            Box::new(FType::Base(BaseType::Unit)),
            Box::new(FType::TypeArrow(
                Box::new(FType::Base(BaseType::Int)),
                Box::new(FType::Base(BaseType::Unit)),
            )),
        ));
    }

    #[test]
    fn convert_fn_1_arg_with_return() {
        // Arrange

        let mini: MiniType = parse_quote!(
            fn(i64) -> bool
        );
        println!("\nMini: {:#?}", &mini);

        // Act
        let converted = mini.convert_type();

        println!("Type: {}", &converted);
        println!("Kind: {}", kind_of(&Context::new(),converted.clone()));

        // Assert
        assert_eq!(converted, FType::TypeArrow(
            Box::new(FType::Base(BaseType::Unit)),
            Box::new(FType::TypeArrow(
                Box::new(FType::Base(BaseType::Int)),
                Box::new(FType::Base(BaseType::Bool)),
            )),
        ));
    }
}