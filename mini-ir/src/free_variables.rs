use std::collections::HashSet;

use crate::Term;
use crate::types::Type;

pub fn free_term_variables(term: Term) -> HashSet<String> {
    match term {
        Term::TermVar(x) => HashSet::from([x]),
        Term::TermAbs(x, typ, t) =>
            &(&free_term_variables(*t) - &HashSet::from([x])) | &free_type_variables(typ),
        Term::TermApp(t1, t2) =>
            &free_term_variables(*t1) | &free_term_variables(*t2),
        Term::TermTypeAbs(x, _, t) =>
            &free_term_variables(*t) - &HashSet::from([x]),
        Term::TermTypeApp(t1, typ) =>
            &free_term_variables(*t1) | &free_type_variables(typ),
        Term::True => HashSet::new(),
        Term::False => HashSet::new(),
        Term::Integer(_) => HashSet::new(),
        Term::Float(_) => HashSet::new(),
        Term::Unit => HashSet::new(),
        Term::Reference(r) => free_term_variables(*r),
        Term::If(t1, t2, t3) => {
            &free_term_variables(*t1) |
                &(&free_term_variables(*t2) |
                    &free_term_variables(*t3))
        }
        Term::Let(x, l, t) =>
            &free_term_variables(*l) | &(&free_term_variables(*t) - &HashSet::from([x])),
        Term::Tuple(t) => t.into_iter().fold(HashSet::new(), |s, t| {
            &s | &free_term_variables(t)
        }),
        Term::TupleProjection(t, _) => free_term_variables(*t),
        Term::Record(t) => t.into_iter().fold(HashSet::new(), |s, (_, t)| {
            &s | &free_term_variables(t)
        }),
        Term::RecordProjection(t, _) => free_term_variables(*t),
        Term::Tagging(_, t, typ) =>
            &free_term_variables(*t) | &free_type_variables(typ),
        Term::Case(t, hmap) =>
            hmap.into_iter().fold(free_term_variables(*t), |s, (_, (x, v))| {
                &s | &(&free_term_variables(v) - &HashSet::from([x]))
            }),
        Term::Fold(typ) | Term::UnFold(typ) => free_type_variables(typ),
        Term::Pack(typ1, t, typ2) =>
            &free_term_variables(*t) | &(&free_type_variables(typ1) | &free_type_variables(typ2)),
        Term::UnPack(x1, x2, t1, t2) =>
            &free_term_variables(*t1) | &(&free_term_variables(*t2) - &HashSet::from([x1, x2])),
        Term::Ascribe(t, typ) =>
            &free_term_variables(*t) | &free_type_variables(typ),
        Term::Define(x, typ, t) =>
            &(&free_term_variables(*t) - &HashSet::from([x])) | &free_type_variables(typ),
        Term::Scope(term) => free_term_variables(*term),
        Term::Seq(term1, term2) =>
            &free_term_variables(*term1) | &free_term_variables(*term2),
        Term::Fix(t) => free_term_variables(*t),
        Term::Class { .. } => { todo!() }
        Term::Instance { .. } => { todo!() }
        Term::Assignment(_, t) => free_term_variables(*t)
    }
}

pub fn free_type_variables(typ: Type) -> HashSet<String> {
    match typ {
        Type::TypeVar(x) => HashSet::from([x]),
        Type::TypeArrow(t1, t2) | Type::TypeApp(t1, t2) =>
            &free_type_variables(*t1) | &free_type_variables(*t2),
        Type::Base(_) => HashSet::new(),
        Type::Reference(t) => free_type_variables(*t),
        Type::Tuple(types) => types.into_iter().fold(HashSet::new(), |s, t| {
            &s | &free_type_variables(t)
        }),
        Type::Record(hmap) | Type::Variants(hmap) =>
            hmap.into_iter().fold(HashSet::new(), |s, (_, t)| { &s | &free_type_variables(t) }),
        Type::Recursive(x, _, t) |
        Type::Existential(x, _, t) |
        Type::TypeAll(x, _, t) |
        Type::TypeAbs(x, _, t) =>
            &free_type_variables(*t) - &HashSet::from([x]),
        Type::Qualified(x, t1) => free_type_variables(*t1),
    }
}

pub fn bound_variable_term(term: Term) -> Option<String> {
    match term {
        Term::Define(name, typ, t) =>
            Some(name),
        _ => None
    }
}

mod tests {
    use std::collections::HashSet;

    use crate::base_type::BaseType;
    use crate::free_variables::{free_term_variables, free_type_variables};
    use crate::kind::Kind::KindStar;
    use crate::Term;
    use crate::types::Type;

    #[test]
    fn forall_type_var() {
        // Arrange
        let t = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::Base(BaseType::Int)));

        // Act
        let vars = free_type_variables(t);

        // Assert
        assert_eq!(vars, HashSet::from([]))
    }

    #[test]
    fn forall_type_var2() {
        // Arrange
        let t = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::TypeVar("X".to_string())));

        // Act
        let vars = free_type_variables(t);

        // Assert
        assert_eq!(vars, HashSet::from([]))
    }

    #[test]
    fn forall_type_var3() {
        // Arrange
        let t = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::TypeVar("Y".to_string())));

        // Act
        let vars = free_type_variables(t);

        // Assert
        assert_eq!(vars, HashSet::from(["Y".to_string()]))
    }

    #[test]
    fn free_var_termvar() {
        // Arrange
        let t = Term::TermVar("x".to_string());

        // Act
        let vars = free_term_variables(t);

        // Assert
        assert_eq!(vars, HashSet::from(["x".to_string()]))
    }

    #[test]
    fn free_var_termabs() {
        // Arrange
        let t = Term::TermAbs("x".to_string(), Type::Base(BaseType::Int), Box::new(Term::TermVar("x".to_string())));

        // Act
        let vars = free_term_variables(t);

        // Assert
        assert_eq!(vars, HashSet::new())
    }

    #[test]
    fn free_var_termabs_unused() {
        // Arrange
        let t = Term::TermAbs("x".to_string(), Type::Base(BaseType::Int), Box::new(Term::TermVar("y".to_string())));

        // Act
        let vars = free_term_variables(t);

        // Assert
        assert_eq!(vars, HashSet::from(["y".to_string()]))
    }
}
