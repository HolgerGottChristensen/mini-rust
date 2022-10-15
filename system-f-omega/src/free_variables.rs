use std::collections::HashSet;
use crate::{Term, Type};

// Todo: Get free variables from types
pub fn free_variables_term(term: Term) -> HashSet<String> {
    match term {
        Term::TermVar(x) => HashSet::from([x]),
        Term::TermAbs(x, typ, t) =>
            &(&free_variables_term(*t) - &HashSet::from([x])) | &free_variables_type(typ),
        Term::TermApp(t1, t2) =>
            &free_variables_term(*t1) | &free_variables_term(*t2),
        Term::TermTypeAbs(x, _, t) =>
            &free_variables_term(*t) - &HashSet::from([x]),
        Term::TermTypeApp(t1, typ) =>
            &free_variables_term(*t1) | &free_variables_type(typ),
        Term::True => HashSet::new(),
        Term::False => HashSet::new(),
        Term::Integer(_) => HashSet::new(),
        Term::Float(_) => HashSet::new(),
        Term::Unit => HashSet::new(),
        Term::Reference(r) => free_variables_term(*r),
        Term::If(t1, t2, t3) => {
            &free_variables_term(*t1)     |
                &(&free_variables_term(*t2) |
                &free_variables_term(*t3))
        }
        Term::Let(x, l, t) =>
            &free_variables_term(*l) | &(&free_variables_term(*t) - &HashSet::from([x])),
        Term::Tuple(t) => t.into_iter().fold(HashSet::new(), |s, t| {
            &s | &free_variables_term(t)
        }),
        Term::TupleProjection(t, _) => free_variables_term(*t),
        Term::Record(t) => t.into_iter().fold(HashSet::new(), |s, (_, t)| {
            &s | &free_variables_term(t)
        }),
        Term::RecordProjection(t, _) => free_variables_term(*t),
        Term::Tagging(_, t, typ) =>
            &free_variables_term(*t) | &free_variables_type(typ),
        Term::Case(t, hmap) =>
            hmap.into_iter().fold(free_variables_term(*t), |s, (_, (x, v))| {
                &s | &(&free_variables_term(v) - &HashSet::from([x]))
            }),
        Term::Fold(typ) | Term::UnFold(typ) => free_variables_type(typ),
        Term::Pack(typ1, t, typ2) =>
            &free_variables_term(*t) | &(&free_variables_type(typ1) | & free_variables_type(typ2)),
        Term::UnPack(x1, x2, t1, t2) =>
            &free_variables_term(*t1) | &(&free_variables_term(*t2) - &HashSet::from([x1, x2])),
        Term::Ascribe(t, typ) =>
            &free_variables_term(*t) | &free_variables_type(typ),
        Term::Define(x, typ, t) =>
            &(&free_variables_term(*t) - &HashSet::from([x])) | &free_variables_type(typ),
        Term::Scope(term) => free_variables_term(*term),
        Term::Seq(term1, term2) =>
            &free_variables_term(*term1) | &free_variables_term(*term2),
    }
}

pub fn free_variables_type(typ: Type) -> HashSet<String> {
    match typ {
        Type::TypeVar(x) => HashSet::from([x]),
        Type::TypeArrow(t1, t2) | Type::TypeApp(t1, t2) =>
            &free_variables_type(*t1) | &free_variables_type(*t2),
        Type::Base(_) => HashSet::new(),
        Type::Reference(t) => free_variables_type(*t),
        Type::Tuple(types) => types.into_iter().fold(HashSet::new(), |s, t| {
            &s | &free_variables_type(t)
        }),
        Type::Record(hmap) | Type::Variants(hmap) =>
            hmap.into_iter().fold(HashSet::new(), |s, (_, t)| { &s | &free_variables_type(t) }),
        Type::Recursive(x, _, t) |
        Type::Existential(x, _, t) |
        Type::TypeAll(x, _, t) |
        Type::TypeAbs(x, _, t) =>
            &free_variables_type(*t) - &HashSet::from([x]),
    }
}

mod tests {
    use std::collections::HashSet;
    use crate::free_variables::free_variables_term;
    use crate::{Int, Term, Type};

    #[test]
    fn free_var_termvar() {
        //arrange
        let t = Term::TermVar("x".to_string());

        //act
        let vars = free_variables_term(t);

        //assert
        assert_eq!(vars, HashSet::from(["x".to_string()]))
    }

    #[test]
    fn free_var_termabs() {
        //arrange
        let t = Term::TermAbs("x".to_string(), Type::Base(Int), Box::new(Term::TermVar("x".to_string())));

        //act
        let vars = free_variables_term(t);

        //assert
        assert_eq!(vars, HashSet::new())
    }

    #[test]
    fn free_var_termabs_unused() {
        //arrange
        let t = Term::TermAbs("x".to_string(), Type::Base(Int), Box::new(Term::TermVar("y".to_string())));

        //act
        let vars = free_variables_term(t);

        //assert
        assert_eq!(vars, HashSet::from(["y".to_string()]))
    }
}
