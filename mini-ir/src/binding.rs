use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use paris::formatter::colorize_string;

use crate::kind::Kind;
use crate::{Context, Term};
use crate::types::Type;

#[derive(Clone, Debug)]
pub enum Binding {
    NameBinding(String), // Todo: Is this even used anymore???
    VarBinding(String, Type),
    TyVarBinding(String, Kind),
}


impl PartialEq<Binding> for &String {
    fn eq(&self, other: &Binding) -> bool {
        match other {
            Binding::NameBinding(s) |
            Binding::VarBinding(s, _) |
            Binding::TyVarBinding(s, _) => s == *self,
            _ => false,
        }
    }
}

impl PartialEq<Binding> for String {
    fn eq(&self, other: &Binding) -> bool {
        match other {
            Binding::NameBinding(s) |
            Binding::VarBinding(s, _) |
            Binding::TyVarBinding(s, _) => s == self,
            _ => false,
        }
    }
}

pub enum BindingDebug {
    Var(String, Type),
    TyVar(String, Kind),
}

impl Debug for BindingDebug {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BindingDebug::Var(n, v) => {write!(f, "{} ↦ ({})", colorize_string(format!("<bright-white>{}</>", n)), v.to_string_type(&Context::new(), 0))}
            BindingDebug::TyVar(n, v) => {write!(f, "{} ↦ ({})", colorize_string(format!("<bright-white>{}</>", n)), v.to_string_kind())}
        }
    }
}