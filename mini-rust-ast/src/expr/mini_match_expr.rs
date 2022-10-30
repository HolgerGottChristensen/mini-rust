use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use syn::{braced, Pat, Token, token};
use syn::parse::{Parse, ParseStream};
use mini_ir::Term;

use crate::mini_expr::MiniExpr;
use crate::mini_pat::{MiniPat, multi_pat_with_leading_vert};
use crate::mini_path::MiniPath;
use crate::ToMiniIrTerm;

#[derive(PartialEq, Clone)]
pub struct MiniExprMatch {
    pub match_token: Token![match],
    pub expr: Box<MiniExpr>,
    pub brace_token: token::Brace,
    pub arms: Vec<MiniArm>,
}

#[derive(PartialEq, Clone)]
pub struct MiniArm {
    pub pat: Pat,
    pub fat_arrow_token: Token![=>],
    pub body: Box<MiniExpr>,
    pub comma: Option<Token![,]>,
}

impl Debug for MiniExprMatch {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MiniExprMatch")
            .field("expr", &self.expr)
            .field("arms", &self.arms)
            .finish()
    }
}

impl Parse for MiniExprMatch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let match_token: Token![match] = input.parse()?;
        let expr = MiniExpr::parse_without_eager_brace(input)?;

        let content;
        let brace_token = braced!(content in input);

        let mut arms = Vec::new();
        while !content.is_empty() {
            arms.push(content.call(MiniArm::parse)?);
        }

        Ok(MiniExprMatch {
            match_token,
            expr: Box::new(expr),
            brace_token,
            arms,
        })
    }
}

impl Debug for MiniArm {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MiniArm")
            .field("pat", &MiniPat(self.pat.clone()))
            .field("body", &self.body)
            .finish()
    }
}

impl Parse for MiniArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let requires_comma;
        Ok(MiniArm {
            pat: multi_pat_with_leading_vert(input)?,
            fat_arrow_token: input.parse()?,
            body: {
                let body = input.call(MiniExpr::parse_expr_early)?;
                requires_comma = MiniExpr::requires_terminator(&body);
                Box::new(body)
            },
            comma: {
                if requires_comma && !input.is_empty() {
                    Some(input.parse()?)
                } else {
                    input.parse()?
                }
            },
        })
    }
}

impl ToMiniIrTerm for MiniExprMatch {
    fn convert_term(&self) -> Term {
        let mut cases = HashMap::new();

        for arm in &self.arms {
            match &arm.pat {
                Pat::TupleStruct(t) => {
                    assert_eq!(t.pat.elems.len(), 1);

                    if let Some(s) = t.pat.elems.first() {
                        match s {
                            Pat::Ident(p) => {
                                if cases.insert(MiniPath(t.path.clone()).as_ident(), (p.ident.to_string(), arm.body.convert_term())).is_some() {
                                    panic!("You can not have the same label more than once");
                                }
                            }
                            x => panic!("Expected ident, found: {:?}", x)
                        }
                    }
                }
                Pat::Ident(y) => {
                    if cases.insert(y.ident.to_string(), ("_".to_string(), arm.body.convert_term())).is_some() {
                        panic!("You can not have the same label more than once");
                    }
                }
                x => panic!("Only tuple structs or idents are allowed, found: {:?}", x)
            }
        }

        Term::Case(Box::new(self.expr.convert_term()), cases)
    }
}

mod tests {
    use paris::log;
    use syn::parse_quote;
    use mini_ir::{Context, Substitutions, type_of, Type as IRType};
    use crate::mini_file::MiniFile;
    use crate::stmt::MiniBlock;
    use crate::ToMiniIrTerm;

    #[test]
    fn match_enum() {
        // Arrange
        let mini: MiniFile = parse_quote!(
            enum Test {
                T1(bool)
            }

            fn main() -> bool {
                let v = Test::T1(true);

                match v {
                    T1(x) => x,
                }
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

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new());
        log!("<blue>======= Type =======</>");
        println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

        log!("<blue>====================</>\n");
        // Assert
        assert!(matches!(converted_type, Ok(..)))
    }

    #[test]
    fn match_enum_empty() {
        // Arrange
        let mini: MiniFile = parse_quote!(
            enum Test {
                T1,
            }

            fn main() -> i64 {
                let v = Test::T1;

                match v {
                    T1 => 1,
                }
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

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new());
        log!("<blue>======= Type =======</>");
        println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

        log!("<blue>====================</>\n");
        // Assert
        assert!(matches!(converted_type, Ok(..)))
    }

    #[test]
    #[should_panic]
    fn match_the_same_twice() {
        // Arrange
        let mini: MiniFile = parse_quote!(
            enum Test {
                T1,
            }

            fn main() -> i64 {
                let v = Test::T1;

                match v {
                    T1 => 1,
                    T1 => 2,
                }
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

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new());
        log!("<blue>======= Type =======</>");
        println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

        log!("<blue>====================</>\n");
        // Assert
        assert!(matches!(converted_type, Err(..)))
    }

    #[test]
    fn match_enum_empty2() {
        // Arrange
        let mini: MiniFile = parse_quote!(
            enum Test {
                T0,
                T1,
            }

            fn main() -> i64 {
                let v = Test::T1;

                match v {
                    T1 => 1,
                }
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

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new());
        log!("<blue>======= Type =======</>");
        println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

        log!("<blue>====================</>\n");
        // Assert
        assert!(matches!(converted_type, Err(..)))
    }

    #[test]
    fn match_enum_empty3() {
        // Arrange
        let mini: MiniFile = parse_quote!(
            enum Test {
                T0,
            }

            fn main() -> i64 {
                let v = Test::T0;

                match v {
                    T0 => 0,
                    T1 => 1,
                }
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

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new());
        log!("<blue>======= Type =======</>");
        println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

        log!("<blue>====================</>\n");
        // Assert
        assert!(matches!(converted_type, Err(..)))
    }
}