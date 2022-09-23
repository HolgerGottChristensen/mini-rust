use crate::mini_expr::{MiniBinOp, MiniExpr};

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprBinary {
    pub left: Box<MiniExpr>,
    pub op: MiniBinOp,
    pub right: Box<MiniExpr>,
}