use std::collections::VecDeque;
use crate::{Binding, Context, Term, Type};


pub fn empty_context() -> Context {
    VecDeque::new()
}

pub fn context_length(context: Context) -> usize {
    context.len()
}

pub fn add_binding(context: &Context, x: String, binding: Binding) -> Context {
    let mut new_context = (*context).clone();
    new_context.push_back((x, binding));
    new_context
}

pub fn add_name(context: &Context, x: String) -> Context {
    add_binding(context, x, Binding::NameBinding)
}

pub fn is_name_bound(context: &Context, x: &String) -> bool {
    for (name, _) in context {
        if x == name {
            return true;
        }
    }

    false
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
}

pub fn type_map(on_var: &dyn Fn(i64, i64, i64) -> Type, c: i64, ty: Type) -> Type {
    match ty {
        Type::TypeVar(x, n) => on_var(c, x, n),
        Type::TypeArrow(ty1, ty2) => Type::TypeArrow(
            Box::new(type_map(on_var, c, *ty1)),
            Box::new(type_map(on_var, c, *ty2))
        ),
        Type::TypeAbs(tyX, knK1, tyT2) =>
            Type::TypeAbs(tyX, knK1, Box::new(type_map(on_var, c + 1, *tyT2))),
        Type::TypeApp(tyT1, tyT2) => Type::TypeApp(
            Box::new(type_map(on_var, c, *tyT1)),
            Box::new(type_map(on_var, c, *tyT2)),
        ),
        Type::TypeAll(tyX, knK1, tyT2) => Type::TypeAll(
            tyX,
            knK1,
            Box::new(type_map(on_var, c + 1, *tyT2))
        )
    }
}

pub fn term_map(on_var: &dyn Fn(i64, i64, i64) -> Term, on_type: &dyn Fn(i64, Type) -> Type, c: i64, term: Term) -> Term {
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

pub fn type_substitution(tys: Type, j: i64, tyt: Type) -> Type {

    let on_var = move |j: i64, x: i64, n: i64| -> Type {
        if x == j {
            type_shift(j, tys.clone())
        } else {
            Type::TypeVar(x, n)
        }
    };

    type_map(&on_var, j, tyt)
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
}

pub fn get_binding(context: &Context, i: i64) -> Binding {
    context[i as usize].1.clone()
}

pub fn get_type_from_context(context: &Context, i: i64) -> Type {
    match get_binding(context, i) {
        Binding::VarBinding(ty) => ty,
        _ => panic!("Binding at position {} is not a VarBinding: {:?}", i, context)
    }
}