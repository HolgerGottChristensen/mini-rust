use crate::types::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Constraint {
    pub ident: String,
    pub vars: Vec<Type>,
}
