use syn::token;

use mini_ir::{Term};

use crate::mini_expr::MiniExpr;
use crate::mini_path::MiniPath;
use crate::ToMiniIrTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprAssign {
    pub left: Box<MiniExpr>,
    pub eq_token: token::Eq,
    pub right: Box<MiniExpr>,
}

impl ToMiniIrTerm for MiniExprAssign {
    fn convert_term(&self) -> Term {
        let ident = match &*self.left {
            MiniExpr::Path(p) => {
                if p.path.get_ident().is_some() {
                    MiniPath(p.path.clone()).as_ident()
                } else {
                    panic!("The path is not a valid ident.")
                }
            }
            _ => panic!("For now only idents can be assigned to"),
        };

        Term::Assignment(ident, Box::new(self.right.convert_term()))
    }
}

mod tests {
    use syn::parse_quote;

    use mini_ir::{BaseType, Context, kind_of, Type, type_of};
    use crate::item::MiniFn;

    use crate::ToMiniIrTerm;
    use crate::stmt::MiniBlock;

    #[test]
    fn parse_simple_assignment() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                let x = 0;
                x = 1
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        assert_eq!(converted_type, Type::Base(BaseType::Int))
    }

    #[test]
    #[should_panic]
    fn parse_simple_assignment_type_fail() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                let x = 0;
                x = true
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        assert_eq!(converted_type, Type::Base(BaseType::Int))
    }

    #[test]
    fn parse_simple_assignment_nested() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                let x = 0;
                {
                    x = 1
                }
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        assert_eq!(converted_type, Type::Base(BaseType::Int))
    }

    #[test]
    fn parse_simple_assignment_shadowing() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                let x = 0;
                {
                    let x = true;
                    x = false;
                };
                x = 1
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        assert_eq!(converted_type, Type::Base(BaseType::Int))
    }

    #[test]
    fn parse_simple_assignment_shadowing2() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                let x = 0;
                let y = {
                    let x = true;
                    x
                };
                y
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        assert_eq!(converted_type, Type::Base(BaseType::Bool))
    }

    #[test]
    fn parse_simple_assignment_shadowing3() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                let x = 0;
                {
                    let x = true;
                    x
                }
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        assert_eq!(converted_type, Type::Base(BaseType::Bool))
    }

    #[test]
    fn parse_simple_assignment_shadowing4() {
        // Arrange
        let mini: MiniFn = parse_quote!(
            fn scoping() {
                {
                    let x = 0;
                };
                x
            }
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&Context::new(), converted.clone()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&Context::new(), converted_type.clone()).unwrap();;
        println!("Kind: {}", &converted_kind);

        // Assert
        assert_eq!(converted_type, Type::Base(BaseType::Bool))
    }
}