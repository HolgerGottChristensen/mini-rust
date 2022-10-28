use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use paris::formatter::colorize_string;
use crate::{Constraint, Kind, Term, Type};
use crate::LinkedList::{Cons, Nil};
use crate::Type::TypeVar;


pub static COLOR_NAMES: &'static [&'static str] = &[
    "red",
    "bright-green",
    "bright-yellow",
    "blue",
    "cyan",
    "magenta"
];

pub fn get_color(index: u32, s: &str) -> String {
    colorize_string(format!("<{}>{}</>", COLOR_NAMES[index as usize % COLOR_NAMES.len()], s))
}


pub enum LinkedList<T> where T: Clone {
    Cons(Rc<(T, LinkedList<T>)>),
    Nil,
}


pub struct LinkedListIter<T: Clone>(LinkedList<T>);

impl<T: Clone> Iterator for LinkedListIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.0 {
            Cons(t) => {
                let res = t.deref().0.clone();
                *self = LinkedListIter(t.deref().1.clone());
                Some(res)
            }
            Nil => None
        }
    }
}

impl<T: Debug + Clone> Debug for LinkedList<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(
            LinkedListIter(self.clone())
        ).finish()
    }
}

impl<T: Clone> Clone for LinkedList<T> {
    fn clone(&self) -> Self {
        match self {
            Cons(s) => Cons(s.clone()),
            Nil => Nil
        }
    }
}

impl<T: Clone> LinkedList<T> {
    pub fn new() -> Self {
        Nil
    }

    pub fn add(&self, element: T) -> Self {
        Cons(Rc::new((element, self.clone() )))
    }

    pub fn len(&self) -> usize {
        match self {
            Cons(content) => 1 + content.1.len(),
            Nil => 0
        }
    }

    pub fn contains<U: PartialEq<T>>(&self, element: U) -> bool {
        match self {
            Cons(cons) => element == cons.0 || cons.1.contains(element),
            Nil => false
        }
    }

    pub fn get<U: PartialEq<T>>(&self, element: U) -> Option<T> {
        match self {
            Cons(cons) => {
                if element == cons.0 {
                    Some(cons.0.clone())
                } else {
                    cons.1.get(element)
                }
            }
            Nil => None,
        }
    }

    /// Get all elements matching a parameter. This will return the innermost defined value first.
    pub fn get_all<U: PartialEq<T> + 'static>(&self, element: U) -> impl Iterator<Item=T> {
        LinkedListIter(self.clone()).filter(move |a| element == *a)
    }
}

#[derive(Debug)]
pub struct Context {
    typing_context: LinkedList<Binding>,
    pub constraints: Rc<RefCell<HashMap<String, Vec<String>>>>,
}

impl Context {

    pub fn new() -> Context {
        Context {
            typing_context: LinkedList::new(),
            constraints: Default::default()
        }
    }

    pub fn add_binding(&self, binding: Binding) -> Self {
        Context {
            typing_context: self.typing_context.add(binding),
            constraints: self.constraints.clone(),
        }
    }

    pub fn add_bindings(&self, bindings: Vec<Binding>) -> Self {

        let mut new = Context {
            typing_context: self.typing_context.clone(),
            constraints: self.constraints.clone(),
        };

        for binding in bindings.into_iter() {
            new = Context {
                typing_context: new.typing_context.add(binding.clone()),
                constraints: new.constraints.clone(),
            }
        }

        new

    }

    pub fn insert_constraint(&mut self, var: &String, classname: &String) {
        let mut constraints = self.constraints.borrow_mut().remove(var).unwrap_or(Vec::new());
        self.insert_constraint_(&mut constraints, classname);
        self.constraints.borrow_mut().insert(var.clone(), constraints);
    }

    fn insert_constraint_(&mut self, constraints: &mut Vec<String>, classname: &String) {
        let mut ii = 0;
        while ii < constraints.len() {
            if &constraints[ii] == classname || self.exists_as_super_class(&constraints[ii], classname) {
                //'classname' was already in the list, or existed as a sub class to the element
                return;
            }
            if self.exists_as_super_class(classname, &constraints[ii]) {
                //There is already a constraint which is a super class of classname,
                //replace that class with this new one
                constraints[ii] = classname.clone();
                return;
            }
            ii += 1;
        }
        constraints.push(classname.clone());
    }

    ///Checks if 'classname' exists as a super class to any
    fn exists_as_super_class(&self, constraint: &String, classname: &String) -> bool {
        let (constraints, _, _) = self.class_items(constraint);

        constraints.iter()
            .any(|super_class| &super_class.ident == classname
                || self.exists_as_super_class(&super_class.ident, classname))
    }

    pub fn class_items(&self, name: &String) -> (Vec<Constraint>, Vec<Type>, HashMap<String, Type>) {
        match self.typing_context.get(name) {
            Some(Binding::ClassBinding { constraints, name, vars, declarations, default_implementations }) => {
                (constraints, vars, declarations)
            }
            _ => panic!("The binding was not a class binding or did not exist.")
        }
    }

    /// Returns whether the type 'searched_type' has an instance for 'class'
    /// If no instance was found, return the instance which was missing
    pub(crate) fn has_instance(&self, class: &String, searched_type: &Type, new_constraints: &mut Vec<Constraint>) -> Result<(), String> {
        println!("{} {} {} {}", colorize_string("<blue>Search after instance:</>"), class, colorize_string("<blue>for</>"), searched_type.to_string_type(&self, 0));

        // Loop through each instance in our local typing environment.
        for ref binding in LinkedListIter(self.typing_context.clone()) {
            if let Binding::InstanceBinding { constraints, class_name, ty, implementations } = binding {
                // If the name is the name of the class we are looking for
                if class == class_name {
                    // We need to check the instances constraints.
                    let result = self.check_instance_constraints(&**constraints, ty, searched_type, new_constraints);

                    // If the instances constraints are upheld, then we can return the instance.
                    // Otherwise we keep on searching.
                    if result.is_ok() {
                        println!("Found and instance of: {} for {:?}", class, searched_type);
                        return result;
                    }
                }
            }
        }

        Err(format!("Could not find instance of: {} for {:?}", class, searched_type))
    }

    fn check_instance_constraints(
        &self,
        constraints: &[Constraint],
        instance_type: &Type,
        actual_type: &Type,
        new_constraints: &mut Vec<Constraint>
    ) -> Result<(), String> {
        println!("Check instance constraints {:?}, actual: {:?}", instance_type, actual_type);
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
                                            vars: vec![TypeVar(var.clone())]
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
            },
            _ => Err("Instance constraints did not match".to_string())
        }
    }

    /// Finds the potential candidates for types with the name in preferred order.
    /// From typecheck.rs: find_fresh
    pub fn potential_candidates(&self, name: &String) -> ZeroOneMore<Type> {
        let mut v = self.typing_context.get_all(name.clone()).filter_map(|a| {
            match a {
                Binding::NameBinding(_) => None,
                Binding::VarBinding(_, t) => Some(t),
                Binding::TyVarBinding(_, _) => None,
                Binding::ClassBinding { .. } => None,
                Binding::InstanceBinding { .. } => None,
            }
        }).collect::<Vec<_>>();

        if v.len() == 0 {
            ZeroOneMore::Zero
        } else if v.len() == 1 {
            ZeroOneMore::One(v.remove(0))
        } else {
            ZeroOneMore::More(v)
        }
    }

    /*fn fresh(&mut self, name: &String) -> Option<Type> {
        match self.find_fresh(name) {
            Some(Type::Qualified(constraints, name)) => {
                let mut subs = Substitutions::new();
                let mut f = Type::Qualified(constraints, name);
                todo!(); /*freshen(self, &mut subs, &mut f);

                for c in constraints.iter() {
                    self.insert_constraint(&c.vars[0].type_var(), c.class.clone());
                }
                Some(typ.value)*/
            }
            Some(_) => None,
            None => None
        }
    }*/
}

pub enum ZeroOneMore<T> {
    Zero,
    One(T),
    More(Vec<T>),
}

pub fn add_name(context: &Context, x: String) -> Context {
    context.add_binding(Binding::NameBinding(x))
}

pub fn is_name_bound(context: &Context, x: &String) -> bool {
    context.typing_context.contains(x)
}

pub fn pick_fresh_name(context: &Context, x: String) -> (Context, String) {
    return if is_name_bound(context, &x) {
        let mut x = x.clone();
        x.push('\'');
        pick_fresh_name(context, x)
    } else {
        (add_name(context, x.clone()), x)
    }
}

/*
pub fn index_to_name(context: &Context, x: usize) -> String {
    context[x].0.clone()
}

pub fn name_to_index(context: &Context, x: &String) -> usize {
    for i in 0..context.len() {
        if &context[i].0 == x {
            return i;
        }
    }

    panic!("The ident is unbound");
}*/
/*
///Freshen creates new type variables at every position where Type::Generic(..) appears.
fn freshen(env: &mut TypeEnvironment, subs: &mut Substitution, typ: &mut Type) {
    debug!("Freshen {:?}", typ);
    fn freshen_(env: &mut TypeEnvironment, subs: &mut Substitution, constraints: &[Constraint], typ: &mut TcType) {
        let result = match *typ {
            Type::Generic(ref id) => freshen_var(env, subs, constraints, id),
            Type::Application(ref mut lhs, ref mut rhs) => {
                freshen_(env, subs, constraints, &mut **lhs);
                freshen_(env, subs, constraints, &mut **rhs);
                None
            }
            _ => None
        };
        match result {
            Some(x) => *typ = x,
            None => ()
        }
    }
    freshen_(env, subs, &*typ.constraints, &mut typ.value);
    for constraint in typ.constraints.iter_mut() {
        match subs.subs.get(&constraint.variables[0]) {
            Some(new) => constraint.variables[0] = new.var().clone(),
            None => ()
        }
    }
}
*/
pub fn type_map(on_var: &dyn Fn(String) -> Type, ty: Type) -> Type {
    match ty {
        Type::TypeVar(s) => on_var(s),
        Type::TypeArrow(ty1, ty2) => Type::TypeArrow(
            Box::new(type_map(on_var,*ty1)),
            Box::new(type_map(on_var,*ty2))
        ),
        Type::TypeAbs(tyX, knK1, tyT2) =>
            Type::TypeAbs(tyX, knK1, Box::new(type_map(on_var,*tyT2))),
        Type::TypeApp(tyT1, tyT2) => Type::TypeApp(
            Box::new(type_map(on_var, *tyT1)),
            Box::new(type_map(on_var, *tyT2)),
        ),
        Type::TypeAll(tyX, knK1, tyT2) => Type::TypeAll(
            tyX,
            knK1,
            Box::new(type_map(on_var, *tyT2))
        ),
        Type::Base(b) => Type::Base(b),
        Type::Reference(t) => {
            Type::Reference(Box::new(type_map(on_var, *t)))
        }
        Type::Tuple(types) => {
            Type::Tuple(types.into_iter().map(|ty| type_map(on_var, ty)).collect::<Vec<_>>())
        }
        Type::Record(types) => {
            Type::Record(HashMap::from_iter(types.into_iter().map(|(label, ty)| (label, type_map(on_var, ty)))))
        }
        Type::Variants(types) => {
            Type::Variants(HashMap::from_iter(types.into_iter().map(|(label, ty)| (label, type_map(on_var, ty)))))
        }
        Type::Recursive(x, kind, ty) => {
            Type::Recursive(x, kind, Box::new(type_map(on_var, *ty)))
        }
        Type::Existential(tyX, knK1, tyT2) => Type::Existential(
            tyX,
            knK1,
            Box::new(type_map(on_var, *tyT2))
        ),
        Type::Qualified(constraint, rest) => {
            todo!()
        },
    }
}

/// Find all variables with the "name", replace with the type "replacement", in type "original"
pub fn type_substitution(name: &String, replacement: Type, original: Type) -> Type {

    let on_var = move |old_name: String| -> Type {
        if old_name == *name {
            replacement.clone()
        } else {
            Type::TypeVar(old_name)
        }
    };

    type_map(&on_var,original)
}

/*pub fn term_map(on_var: &dyn Fn(i64, i64, i64) -> Term, on_type: &dyn Fn(i64, Type) -> Type, c: i64, term: Term) -> Term {
    match term {
        Term::TermVar(x, n) => on_var(c, x, n),
        Term::TermAbs(x, tyT1, t2) => Term::TermAbs(
            x,
            on_type(c, tyT1),
            Box::new(term_map(on_var, on_type, c+1, *t2))
        ),
        Term::TermApp(t1, t2) => Term::TermApp(
            Box::new(term_map(on_var, on_type, c, *t1)),
            Box::new(term_map(on_var, on_type, c, *t2))
        ),
        Term::TermTypeAbs(tyX,knK1,t2) => Term::TermTypeAbs(
            tyX,
            knK1,
            Box::new(term_map(on_var, on_type, c+1, *t2))
        ),
        Term::TermTypeApp(t1,tyT2) => Term::TermTypeApp(
            Box::new(term_map(on_var, on_type, c, *t1)),
            on_type(c, tyT2)
        )
    }
}

pub fn type_shift_above(d: i64, c: i64, ty: Type) -> Type {

    let on_var = move |c: i64, x: i64, n: i64| -> Type {
        if x >= c {
            Type::TypeVar(x + d, n + d)
        } else {
            Type::TypeVar(x, n + d)
        }
    };

    type_map(&on_var, c, ty)
}

pub fn term_shift_above(d: i64, c: i64, term: Term) -> Term {
    let on_var = move |c: i64, x: i64, n: i64| -> Term {
        if x >= c {
            Term::TermVar(x + d, n + d)
        } else {
            Term::TermVar(x, n + d)
        }
    };

    let on_type = |c: i64, ty: Type| -> Type {
        type_shift_above(d, c, ty)
    };

    term_map(&on_var, &on_type, c, term)
}

pub fn type_shift(d: i64, ty: Type) -> Type {
    type_shift_above(d, 0, ty)
}

pub fn term_shift(d: i64, term: Term) -> Term {
    term_shift_above(d, 0, term)
}

pub fn binding_shift(d: i64, bind: Binding) -> Binding {
    match bind {
        Binding::NameBinding => Binding::NameBinding,
        Binding::VarBinding(ty) => Binding::VarBinding(type_shift(d, ty)),
        Binding::TyVarBinding(knK) => Binding::TyVarBinding(knK)
    }
}

pub fn term_substitution(j: i64, s: Term, term: Term) -> Term {

    let on_var = move |j: i64, x: i64, n: i64| -> Term {
        if x == j {
            term_shift(j, s.clone())
        } else {
            Term::TermVar(x, n)
        }
    };

    let on_type = |j: i64, ty: Type| -> Type {
        ty
    };

    term_map(&on_var, &on_type, j, term)
}

pub fn term_substitution_top(s: Term, term: Term) -> Term {

    let gs = term_shift(1, s);

    let g = term_substitution(0, gs, term);

    term_shift(-1, g)
}

pub fn type_substitution_top(tys: Type, tyt: Type) -> Type {

    let gs = type_shift(1, tys);

    let g = type_substitution(gs, 0, tyt);

    type_shift(-1, g)
}

pub fn type_term_substitution(tys: Type, j: i64, term: Term) -> Term {

    let on_var = move |c: i64, x: i64, n: i64| -> Term {
        Term::TermVar(x, n)
    };

    let on_type = |j: i64, ty: Type| -> Type {
        type_substitution(tys.clone(), j, ty)
    };

    term_map(&on_var, &on_type, j, term)
}

pub fn type_term_substitution_top(tys: Type, term: Term) -> Term {
    let gs = type_shift(1, tys);

    let g = type_term_substitution(gs, 0, term);

    term_shift(-1, g)
}*/


pub fn get_binding(context: &Context, name: &String) -> Option<Binding> {
    context.typing_context.get(name)
}

pub fn get_type(context: &Context, name: &String) -> Result<Type, String> {
    match get_binding(context, name) {
        Some(Binding::VarBinding(_, ty)) => Ok(ty),
        _ => Err(format!("Binding with ident '{}' is not a VarBinding in the current Context: {:#?}", name, context))
    }
}


pub fn class_exists(context: &Context, name: &String) -> bool {
    context.typing_context.get(name).is_some()
}

pub fn get_type_safe(context: &Context, name: &String) -> Result<Type, String> {
    match get_binding(context, name) {
        Some(Binding::VarBinding(_, ty)) => Ok(ty),
        _ => Err(format!("Binding with ident '{}' is not a VarBinding in the current Context: {:?}", name, context))
    }
}

pub fn get_kind(context: &Context, name: &String) -> Kind {
    match get_binding(context, name) {
        Some(Binding::TyVarBinding(_, k)) => k,
        _ => panic!("Binding with ident '{}' is not a TyVarBinding in the current Context: {:?}", name, context)
    }
}



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
        default_implementations: HashMap<String, Term>
    },
    InstanceBinding {
        /// The constraints required for a type to be able to use this implementation
        constraints: Vec<Constraint>,
        /// The name of the class
        class_name: String,
        /// The implementation is for this type
        ty: Type,
        /// Implementations of this instance
        implementations: HashMap<String, (Term, Type)>
    },
}


impl PartialEq<Binding> for &String {
    fn eq(&self, other: &Binding) -> bool {
        match other {
            Binding::NameBinding(s) |
            Binding::VarBinding(s, _) |
            Binding::TyVarBinding(s, _) => s == *self,
            Binding::ClassBinding{ name, .. } => name == *self,
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
            Binding::ClassBinding{ name, .. } => name == self,
            _ => false,
        }
    }
}


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
            res = type_substitution(k, sub.clone(), res);
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
                        res = type_substitution(k, sub.clone(), res);
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
