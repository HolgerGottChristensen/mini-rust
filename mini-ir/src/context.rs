use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};

use im_rc::{Vector, vector};
use paris::formatter::colorize_string;
use paris::log;

use crate::binding::Binding;
use crate::BindingDebug;
use crate::constraint::Constraint;
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
                    Binding::ClassBinding { .. } => None,
                    Binding::InstanceBinding { .. } => None,
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

    /// Checks if 'classname' exists as a super class to any
    fn exists_as_super_class(&self, constraint: &String, classname: &String) -> bool {
        let (constraints, _, _) = self.class_items(constraint);

        constraints.iter()
            .any(|super_class| &super_class.ident == classname
                || self.exists_as_super_class(&super_class.ident, classname))
    }

    pub fn class_items(&self, name: &String) -> (Vec<Constraint>, Vec<Type>, HashMap<String, Type>) {
        match self.0.iter().find(|e| name == *e).cloned() {
            Some(Binding::ClassBinding { constraints, name, vars, declarations, default_implementations }) => {
                (constraints, vars, declarations)
            }
            _ => panic!("The binding was not a class binding or did not exist.")
        }
    }

    /// Returns whether the type 'searched_type' has an instance for 'class'
    /// If no instance was found, return the instance which was missing
    pub fn has_instance(&self, class: &String, searched_type: &Type, new_constraints: &mut Vec<Constraint>) -> Result<(), String> {
        log!("<blue>Search after instance:</> {} <blue>for</> {}", class, searched_type.to_string_type(&self, 0));

        // Loop through each instance in our local typing environment.
        for ref binding in self.0.clone().into_iter() {
            if let Binding::InstanceBinding { constraints, class_name, ty, implementations } = binding {
                // If the name is the name of the class we are looking for
                if class == class_name {
                    // We need to check the instances constraints.
                    // Todo: Dont take first element
                    let result = self.check_instance_constraints(&**constraints, &ty[0], searched_type, new_constraints);

                    // If the instances constraints are upheld, then we can return the instance.
                    // Otherwise we keep on searching.
                    if result.is_ok() {
                        log!("<green>Found and instance of:</> {} <green>for</> {}", class, searched_type.to_string_type(&self, 0));
                        return result;
                    }
                }
            }
        }

        log!("<red>Could not find instance of:</> {} <red>for</> {}", class, searched_type.to_string_type(&self, 0));
        Err(format!("Could not find instance of: {} for {}", class, searched_type.to_string_type(&self, 0)))
    }

    fn check_instance_constraints(
        &self,
        constraints: &[Constraint],
        instance_type: &Type,
        actual_type: &Type,
        new_constraints: &mut Vec<Constraint>,
    ) -> Result<(), String> {
        println!("\tCheck instance constraints {:?}, actual: {:?}", instance_type, actual_type);
        match (instance_type, actual_type) {
            // If the instance and the actual type are both type applications
            (
                Type::TypeApp(lvar, r),
                Type::TypeApp(ltype, rtype)
            ) => {
                if let Type::TypeVar(rvar) = &**r {
                    let all_upheld = constraints.iter()
                        // Look only at the constraints that includes the variable as the first constraint variable.
                        .filter(|constraint| constraint.vars[0].type_var() == *rvar)
                        .map(|constraint| {
                            // We need to check that the constraint holds for the inner type
                            let result = self.has_instance(&constraint.ident, &**rtype, new_constraints);

                            // If there was an instance and the actual applied type is a Variable, add that constraint to the new_constraints.
                            if result.is_ok() {
                                match **rtype {
                                    Type::TypeVar(ref var) => {
                                        new_constraints.push(Constraint {
                                            ident: constraint.ident.clone(),
                                            vars: vec![TypeVar(var.clone())],
                                        });
                                    }
                                    _ => ()
                                }
                            }
                            result
                        })
                        .all(|result| result.is_ok());

                    if all_upheld {
                        // If everything is upheld we need to check the lhs if they are also upheld.
                        self.check_instance_constraints(constraints, &**lvar, &**ltype, new_constraints)
                    } else {
                        Err("Some of the constraints did not hold for the actual rtype if replaced into the instance type".to_string())
                    }
                } else {
                    Err("The right hand side of the instance application was not a type variable.".to_string())
                }
            }
            (Type::TypeAbs(l, _, _), Type::TypeAbs(r, _, _)) if l == r => Ok(()),
            (_, Type::TypeVar(_)) => Ok(()),
            (Type::Base(b1), Type::Base(b2)) => {
                if b1 == b2 {
                    Ok(())
                } else {
                    Err("Unknown Base error".to_string())
                }
            }
            _ => Err("Instance constraints did not match".to_string())
        }
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

    pub fn class_exists(&self, name: &String) -> bool {
        self.get_binding(name).is_some()
    }

    pub fn get_kind(&self, name: &String) -> Result<Kind, String> {
        match self.get_binding(name) {
            Some(Binding::TyVarBinding(_, k)) => Ok(k),
            _ => Err(format!("Binding with ident '{}' is not a TyVarBinding in the current Context: {:?}", name, self))
        }
    }
}
