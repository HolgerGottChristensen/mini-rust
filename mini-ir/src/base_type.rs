use std::fmt::{Debug, Formatter};

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum BaseType {
    Int,
    Bool,
    Unit,
    Float,
}

impl Debug for BaseType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseType::Int => write!(f, "Int"),
            BaseType::Bool => write!(f, "Bool"),
            BaseType::Unit => write!(f, "Unit"),
            BaseType::Float => write!(f, "Float"),
        }
    }
}
