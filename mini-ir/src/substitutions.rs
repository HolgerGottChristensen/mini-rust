use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use crate::constraint::Constraint;
use crate::Context;
use crate::types::Type;

/// A Substitution is a mapping from typevariables to types.
#[derive(Clone)]
pub struct Substitutions {
    /// A hashmap which contains what a typevariable is unified to.
    pub subs: HashMap<String, Type>,
}

impl Substitutions {
    /// Apply all substitutions to the given type and return a new type
    pub fn apply(&self, ty: Type) -> Type {
        let mut res = ty;
        for (k, sub) in &self.subs {
            res = crate::type_util::type_substitution(k, sub.clone(), res);
        }

        res
    }

    pub fn apply_consts(&self, consts: Vec<Constraint>) -> Vec<Constraint> {
        consts.into_iter().map(|c| {
            Constraint {
                ident: c.ident,
                vars: c.vars.into_iter().map(|t| {
                    let mut res = t;
                    for (k, sub) in &self.subs {
                        res = crate::type_util::type_substitution(k, sub.clone(), res);
                    }
                    res
                }).collect()
            }
        }).collect()
    }
}

struct SubstitutionDebug(String, Type);

impl Debug for SubstitutionDebug {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ↦ ({})", self.0, self.1.to_string_type(&Context::new(), 0))
    }
}

impl Debug for Substitutions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.subs.iter().map(|(k, v)| {
            SubstitutionDebug(k.to_string(), v.clone())
        })).finish()
    }
}


impl Substitutions {
    pub fn new() -> Substitutions {
        Substitutions {
            subs: HashMap::new()
        }
    }

    pub fn insert(&mut self, var: String, ty: Type) {
        self.subs.insert(var, ty);
    }
}
