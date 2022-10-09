use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use crate::{Binding, Kind, Term, Type};
use crate::LinkedList::{Cons, Nil};

#[derive(Debug)]
pub enum LinkedList<T> where T: Clone {
    Cons(Rc<(T, LinkedList<T>)>),
    Nil,
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
}

pub type Context = LinkedList<Binding>;


pub fn add_binding(context: &Context, binding: Binding) -> Context {
    context.add(binding)
}

pub fn add_name(context: &Context, x: String) -> Context {
    add_binding(context, Binding::NameBinding(x))
}

pub fn is_name_bound(context: &Context, x: &String) -> bool {
    context.contains(x)
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
    context.get(name)
}

pub fn get_type(context: &Context, name: &String) -> Type {
    match get_binding(context, name) {
        Some(Binding::VarBinding(_, ty)) => ty,
        _ => panic!("Binding at position {} is not a VarBinding: {:?}", name, context)
    }
}

pub fn get_kind(context: &Context, name: &String) -> Kind {
    match get_binding(context, name) {
        Some(Binding::TyVarBinding(_, k)) => k,
        _ => panic!("Binding at position {} is not a TyVarBinding: {:?}", name, context)
    }
}