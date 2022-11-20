use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};

use im_rc::{Vector, vector};
use paris::log;

use crate::binding::Binding;
use crate::BindingDebug;
use crate::kind::Kind;
use crate::types::Type;
use crate::types::Type::TypeVar;

#[derive(Debug)]
pub struct Context(Vector<Binding>);

impl Display for Context {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.0.iter().filter_map(|binding| {
                match binding {
                    Binding::NameBinding(_) => None,
                    Binding::VarBinding(v, b) => Some(BindingDebug::Var(v.clone(), b.clone())),
                    Binding::TyVarBinding(v, k) => Some(BindingDebug::TyVar(v.clone(), k.clone())),
                }
            }))
            .finish()
    }
}

impl Context {
    pub fn new() -> Context {
        Context(vector![])
    }

    pub fn add_binding(&self, binding: Binding) -> Self {
        let mut list = self.0.clone();
        list.push_front(binding);
        Context(list)
    }

    pub fn add_bindings(&self, bindings: Vec<Binding>) -> Self {
        let mut new = Context(self.0.clone());

        for binding in bindings.into_iter() {
            new = new.add_binding(binding);
        }

        new
    }

    pub fn add_name(&self, x: String) -> Context {
        self.add_binding(Binding::NameBinding(x))
    }

    pub fn is_name_bound(&self, x: &String) -> bool {
        self.0.iter().any(|e| x == e)
    }

    pub fn pick_fresh_name(&self, x: &String) -> (Context, String) {
        return if self.is_name_bound(x) {
            let mut x = x.clone();
            x.push('\'');
            self.pick_fresh_name(&x)
        } else {
            (self.add_name(x.clone()), x.clone())
        };
    }

    pub fn get_binding(&self, name: &String) -> Option<Binding> {
        self.0.clone().into_iter().find(|e| name == e)
    }

    pub fn get_type(&self, name: &String) -> Result<Type, String> {
        match self.get_binding(name) {
            Some(Binding::VarBinding(_, ty)) => Ok(ty),
            _ => Err(format!("Binding with ident '{}' is not a VarBinding in the current Context: {:#?}", name, self))
        }
    }

    pub fn get_kind(&self, name: &String) -> Result<Kind, String> {
        match self.get_binding(name) {
            Some(Binding::TyVarBinding(_, k)) => Ok(k),
            _ => Err(format!("Binding with ident '{}' is not a TyVarBinding in the current Context: {:?}", name, self))
        }
    }
}
