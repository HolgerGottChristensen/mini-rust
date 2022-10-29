use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use paris::formatter::colorize_string;

use crate::constraint::Constraint;
use crate::kind::Kind;
use crate::{Context, Term};
use crate::types::Type;

#[derive(Clone, Debug)]
pub enum Binding {
    NameBinding(String),
    VarBinding(String, Type),
    TyVarBinding(String, Kind),
    ClassBinding {
        /// The constraints required for implementors of this class
        constraints: Vec<Constraint>,
        /// The name of the class
        name: String,
        /// The type variables for the type
        vars: Vec<Type>,
        /// The implementation type declarations for the class.
        declarations: HashMap<String, Type>,
        /// The default implementations of this class. We also know them as bindings
        default_implementations: HashMap<String, Term>,
    },
    InstanceBinding {
        /// The constraints required for a type to be able to use this implementation
        constraints: Vec<Constraint>,
        /// The name of the class
        class_name: String,
        /// The implementation is for this type
        ty: Vec<Type>,
        /// Implementations of this instance
        implementations: HashMap<String, (Term, Type)>,
    },
}


impl PartialEq<Binding> for &String {
    fn eq(&self, other: &Binding) -> bool {
        match other {
            Binding::NameBinding(s) |
            Binding::VarBinding(s, _) |
            Binding::TyVarBinding(s, _) => s == *self,
            Binding::ClassBinding { name, .. } => name == *self,
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
            Binding::ClassBinding { name, .. } => name == self,
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