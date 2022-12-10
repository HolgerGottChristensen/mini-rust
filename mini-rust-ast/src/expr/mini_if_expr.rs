use syn::parse::{Parse, ParseStream};
use syn::{Token, token};
use mini_ir::Term;
use crate::expr::MiniExprBlock;
use crate::mini_expr::MiniExpr;
use crate::stmt::MiniBlock;
use crate::ToMiniIrTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprIf {
    pub if_token: Token![if],
    pub cond: Box<MiniExpr>,
    pub then_branch: MiniBlock,
    pub else_branch: Option<(Token![else], Box<MiniExpr>)>,
}

impl Parse for MiniExprIf {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(MiniExprIf {
            if_token: input.parse()?,
            cond: Box::new(input.call(MiniExpr::parse_without_eager_brace)?),
            then_branch: input.parse()?,
            else_branch: {
                if input.peek(Token![else]) {
                    Some(input.call(else_block)?)
                } else {
                    None
                }
            },
        })
    }
}


fn else_block(input: ParseStream) -> syn::Result<(Token![else], Box<MiniExpr>)> {
    let else_token: Token![else] = input.parse()?;

    let lookahead = input.lookahead1();
    let else_branch = if input.peek(Token![if]) {
        input.parse().map(MiniExpr::If)?
    } else if input.peek(token::Brace) {
        MiniExpr::Block(MiniExprBlock {
            block: MiniBlock::parse(input)?
        })
    } else {
        return Err(lookahead.error());
    };

    Ok((else_token, Box::new(else_branch)))
}

impl ToMiniIrTerm for MiniExprIf {
    fn convert_term(&self) -> Term {

        if let Some((_, else_branch)) = &self.else_branch {
            Term::IfElse(
                Box::new(self.cond.convert_term()),
                Box::new(self.then_branch.convert_term()),
                Box::new(else_branch.convert_term()),
            )
        } else {
            Term::If(
                Box::new(self.cond.convert_term()),
                Box::new(self.then_branch.convert_term()),
            )
        }
    }
}

mod tests {
    use paris::log;
    use syn::parse_quote;
    use mini_ir::{Context, type_of, Type as IRType, Binding, Type, Unit, Int, Bool};
    use crate::expr::MiniExprIf;
    use crate::stmt::MiniBlock;
    use crate::ToMiniIrTerm;

    #[test]
    fn empty_if_true_no_else() {
        // Arrange
        let mini: MiniExprIf = parse_quote!(
            if true {
                1;
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

        let converted_type = type_of(&context, converted.clone());
        log!("<blue>======= Type =======</>");
        println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

        log!("<blue>====================</>\n");
        // Assert
        assert!(matches!(converted_type, Ok(..)))
    }

    #[test]
    fn empty_if_true_with_else() {
        // Arrange
        let mini: MiniExprIf = parse_quote!(
            if true {
                1
            } else {
                2
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

        let converted_type = type_of(&context, converted.clone());
        log!("<blue>======= Type =======</>");
        println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

        log!("<blue>====================</>\n");
        // Assert
        assert!(matches!(converted_type, Ok(..)))
    }

    #[test]
    fn empty_if_true_with_else_if() {
        // Arrange
        let mini: MiniExprIf = parse_quote!(
            if true {
                1;
            } else if 1 == 3 {
                2;
            }
        );
        let context = Context::new();
        let context = context.add_binding(Binding::VarBinding("eq".to_string(), Type::arrow(Unit, Type::arrow(Int, Type::arrow(Int, Bool)))));


        log!("<blue>======== AST =======</>");
        println!("{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        log!("<blue>====== Lambda ======</>");
        println!("{}", &converted);
        log!("<blue>==== Type-Check ====</>");

        let converted_type = type_of(&context, converted.clone());
        log!("<blue>======= Type =======</>");
        println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

        log!("<blue>====================</>\n");
        // Assert
        assert!(matches!(converted_type, Ok(..)))
    }

    #[test]
    fn empty_if_true_with_else_if_else() {
        // Arrange
        let mini: MiniExprIf = parse_quote!(
            if true {
                1
            } else if false {
                2
            } else {
                3
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

        let converted_type = type_of(&context, converted.clone());
        log!("<blue>======= Type =======</>");
        println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

        log!("<blue>====================</>\n");
        // Assert
        assert!(matches!(converted_type, Ok(..)))
    }

    #[test]
    fn empty_if_true_no_else_nested() {
        // Arrange
        let mini: MiniExprIf = parse_quote!(
            if true {
                if true {
                    1;
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

        let converted_type = type_of(&context, converted.clone());
        log!("<blue>======= Type =======</>");
        println!("{}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

        log!("<blue>====================</>\n");
        // Assert
        assert!(matches!(converted_type, Ok(..)))
    }
}