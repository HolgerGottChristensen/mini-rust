use syn::{braced, Token};
use syn::parse::{Parse, ParseStream};
use mini_ir::Term;

use crate::mini_expr::MiniExpr;
use crate::stmt::{MiniBlock, parse_within};
use crate::ToMiniIrTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprWhile {
    pub while_token: Token![while],
    pub cond: Box<MiniExpr>,
    pub body: MiniBlock,
}

impl Parse for MiniExprWhile {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let while_token: Token![while] = input.parse()?;
        let cond = MiniExpr::parse_without_eager_brace(input)?;

        let content;
        let brace_token = braced!(content in input);
        let stmts = content.call(parse_within)?;

        Ok(MiniExprWhile {
            while_token,
            cond: Box::new(cond),
            body: MiniBlock { brace_token, stmts },
        })
    }
}

impl ToMiniIrTerm for MiniExprWhile {
    fn convert_term(&self) -> Term {
        Term::While(Box::new(self.cond.convert_term()), Box::new(self.body.convert_term()), Box::new(Term::Replacement))
    }
}

mod tests {
    use paris::log;
    use syn::parse_quote;
    use mini_ir::{Context, Substitutions, type_of};
    use crate::stmt::MiniBlock;
    use crate::ToMiniIrTerm;

    #[test]
    fn empty_while_true() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                while true { }
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
    fn empty_while_one() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                while 1 { }
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
    fn empty_while_return_non_unit() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                while true {
                    1
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
    fn empty_while_x() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                while x {}
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
    fn empty_while_x_in_function() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                fn test(t: bool) {
                    while t {

                    }
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
}

