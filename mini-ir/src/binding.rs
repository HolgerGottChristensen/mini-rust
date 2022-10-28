use std::collections::HashMap;

use crate::constraint::Constraint;
use crate::kind::Kind;
use crate::Term;
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
        ty: Type,
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
