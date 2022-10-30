use syn::parse::ParseStream;

use mini_ir::Term;

use crate::mini_expr::{AllowStruct, MiniExpr, MiniUnOp};
use crate::ToMiniIrTerm;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprUnary {
    pub op: MiniUnOp,
    pub expr: Box<MiniExpr>,
}


pub fn parse_expr_unary(
    input: ParseStream,
    allow_struct: AllowStruct,
) -> syn::Result<MiniExprUnary> {
    Ok(MiniExprUnary {
        op: input.parse()?,
        expr: Box::new(MiniExpr::parse_unary_expr(input, allow_struct)?),
    })
}

impl ToMiniIrTerm for MiniExprUnary {
    fn convert_term(&self) -> Term {
        // Todo: How do we access methods in traits based on some parameters. We might need an environment and lookup a function?
        match self.op {
            MiniUnOp::Deref(_) => {
                Term::DeReference(Box::new(self.expr.convert_term()))
            }
            MiniUnOp::Not(_) => {
                // Todo: We need to be able to differentiate between not for the different types
                Term::TermApp(
                    Box::new(Term::TermVar("not".to_string())),
                    Box::new(self.expr.convert_term()),
                )
            }
            MiniUnOp::Neg(_) => {
                // Todo: We need to be able to differentiate between neg for the different types
                Term::TermApp(
                    Box::new(Term::TermVar("neg".to_string())),
                    Box::new(self.expr.convert_term()),
                )
            }
        }
    }
}


mod tests {
    use paris::log;
    use syn::parse_quote;
    use mini_ir::{Context, Substitutions, type_of, Type as IRType};
    use crate::stmt::MiniBlock;
    use crate::ToMiniIrTerm;

    #[test]
    fn deref_reference() {
        // Arrange
        let mini: MiniBlock = parse_quote!(
            {
                *&true
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