use std::fmt::{Debug, Formatter};

use paris::formatter::colorize_string;
use quote::ToTokens;
use syn::Lit;
use syn::parse::{Parse, ParseStream};

use mini_ir::Term;

use crate::ToMiniIrTerm;

#[derive(PartialEq, Clone)]
pub struct MiniLitExpr {
    pub lit: Lit,
}

impl Debug for MiniLitExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = colorize_string(format!("<green>{}</>", self.lit.to_token_stream().to_string()));
        write!(f, "{}", s)
    }
}

impl Parse for MiniLitExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lit = Lit::parse(input)?;

        Ok(MiniLitExpr {
            lit
        })
    }
}

impl ToMiniIrTerm for MiniLitExpr {
    fn convert_term(&self) -> Term {
        match &self.lit {
            Lit::Str(_) => todo!(),
            Lit::ByteStr(_) => todo!(),
            Lit::Byte(_) => todo!(),
            Lit::Char(_) => todo!(),
            Lit::Int(i) => {
                Term::Integer(i.base10_parse::<i64>().unwrap())
            }
            Lit::Float(f) => {
                Term::Float(f.base10_parse::<f64>().unwrap())
            }
            Lit::Bool(b) => {
                if b.value {
                    Term::True
                } else {
                    Term::False
                }
            }
            Lit::Verbatim(_) => todo!(),
        }
    }
}

mod tests {
    use syn::parse_quote;

    use mini_ir::{Context, kind_of, Substitutions, Term, type_of};

    use crate::{MiniLitExpr, ToMiniIrTerm};

    #[test]
    fn parse_lit_int() {
        // Arrange
        let mini: MiniLitExpr = parse_quote!(
            0
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&Context::new(), converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        assert_eq!(converted, Term::Integer(0))
    }

    #[test]
    fn parse_lit_float() {
        // Arrange
        let mini: MiniLitExpr = parse_quote!(
            3.141592
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&Context::new(), converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", converted_type);
        println!("Kind: {}", converted_kind);

        // Assert
        assert_eq!(converted, Term::Float(3.141592))
    }

    #[test]
    fn parse_lit_bool_true() {
        // Arrange
        let mini: MiniLitExpr = parse_quote!(
            true
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&Context::new(), converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", converted_type);
        println!("Kind: {}", converted_kind);

        // Assert
        assert_eq!(converted, Term::True)
    }

    #[test]
    fn parse_lit_bool_false() {
        // Arrange
        let mini: MiniLitExpr = parse_quote!(
            false
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&Context::new(), converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", converted_type);
        println!("Kind: {}", converted_kind);

        // Assert
        assert_eq!(converted, Term::False)
    }
}