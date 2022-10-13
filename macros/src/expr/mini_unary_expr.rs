use syn::parse::ParseStream;
use system_f_omega::Term;
use crate::mini_expr::{AllowStruct, MiniExpr, MiniUnOp};
use crate::ToSystemFOmegaTerm;

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

impl ToSystemFOmegaTerm for MiniExprUnary {
    fn convert_term(&self) -> Term {
        // Todo: How do we access methods in traits based on some parameters. We might need an environment and lookup a function?
        match self.op {
            MiniUnOp::Deref(_) => {
                todo!()
            }
            MiniUnOp::Not(_) => {
                // Todo: We need to be able to differentiate between not for the different types
                Term::TermApp(
                    Box::new(Term::TermVar("not".to_string())),
                    Box::new(self.expr.convert_term())
                )
            }
            MiniUnOp::Neg(_) => {
                // Todo: We need to be able to differentiate between neg for the different types
                Term::TermApp(
                    Box::new(Term::TermVar("neg".to_string())),
                    Box::new(self.expr.convert_term())
                )
            }
        }
    }
}