use std::collections::{BTreeSet, HashMap, HashSet};
use std::collections::hash_map::RandomState;
use crate::{Context, get_inst, get_type, get_type_safe, Term, Type, type_equivalence, type_substitution};

// Todo: Get free variables from types
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
            &free_term_variables(*t1)     |
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
            &free_term_variables(*t) | &(&free_type_variables(typ1) | & free_type_variables(typ2)),
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
        Term::Overload(_, _, _) => {todo!()}
        Term::Instance(_, _, _, _) => {todo!()}
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
        Type::Predicate(x, t1, t2) =>
            &(&free_type_variables(*t1) | &free_type_variables(*t2)) - &HashSet::from([x]),
    }
}

/// Check if T2 is a specialization of T1, formally: T1 ⊑ T2
pub fn type_specialization(t1: Type, t2: Type, context: &Context) -> bool {

    // ∀X1...∀Xn. (x1 :: r).(xn :: r). T1
    let (prefixed_t1, rest1) = prefixed_for_all_vars(t1.clone()); // [X1, X2, ..., Xn]  |  (x1 :: r).(xn :: r). T1
    let (prefixed_t2, rest2) = prefixed_for_all_vars(t2.clone());
    let fv_t1 = free_type_variables(t1.clone());

    // Check (1) that no variables in T2 is free variables in T1
    for p in &prefixed_t2 {
        if fv_t1.contains(p) {
            panic!("The forall variables in T2 must not be a free variable in T1")
        }
    }

    // Check (2) create a list of substitution for T1 that makes it equal T2
    let substitutions = substitutions(rest1.clone(), rest2.clone(), &prefixed_t1);


    // Check if there are any conflicting substitutions
    let hashmap = HashMap::<String, Type, RandomState>::from_iter(substitutions.clone());
    for (lab, typ) in &substitutions {
        let t = &hashmap[lab];
        if !type_equivalence(context, typ.clone(), t.clone()) {
            panic!("There are conflicting substitutions in the list, hence t2 is not a specialization of t1")
        }
    }


    //dbg!(&rest1);
    // Apply substitutions to t1
    let mut new_t1 = rest1;
    let mut new_t2 = rest2;

    for (k, v) in &hashmap {
        new_t1 = type_substitution(k, v.clone(), new_t1);
    }
    //dbg!(&new_t1);


    // (x1 :: r).(xn :: r). T1
    let (predicates_t1, rest1) = prefixed_predicates(new_t1.clone()); // [(x1 :: r), ..., (xn :: r)] | T1
    let (predicates_t2, rest2) = prefixed_predicates(new_t2.clone()); // [(x1 :: r), ..., (xn :: r)] | T2

    //dbg!(&predicates_t1);
    //dbg!(&predicates_t2);

    let predicates_t1 = predicates_t1.iter().filter(|(a, ty1)| {
        let mut keep = true;

        // If the predicate is in both types, we can remove it from the list.
        for (b, ty2) in &predicates_t2 {
            if a == b && type_equivalence(context, ty1.clone(), ty2.clone()) {
                keep = false;
            }
        }

        keep
    }).cloned().collect::<Vec<_>>();

    //dbg!(&predicates_t1);

    let predicates_t1 = predicates_t1.iter().filter(|(a, ty1)| {
        let mut keep = true;

        // If the type is defined in the context with the expected type, we can remove it.
        match get_type_safe(context, a) {
            Ok(context_type) => {
                if type_equivalence(context, context_type, ty1.clone()) {
                    keep = false;
                }
            }
            Err(_) => {}
        }

        //dbg!(context);
        // If the type is defined in an inst, we can remove it if the inst type is more general than the expected type.
        match get_inst(context, a) {
            Ok(ctx_type) => {

                if type_specialization(ctx_type, ty1.clone(), context) {
                    keep = false;
                }
            }
            Err(_) => {}
        }

        keep
    }).cloned().collect::<Vec<_>>();

    //dbg!(&predicates_t1);

    if predicates_t1.len() != 0 {
        panic!("We expected the predicates list to be empty");
    }

    if !type_equivalence(context, rest1.clone(), rest2.clone()) {
        panic!("The rest of t1: {} and t2: {}, are not equal", &rest1, &rest2);
    }

    true
}

pub fn predicates(t1: Type) -> Vec<(String, Type)> {
    match t1 {
        Type::TypeVar(_) => vec![],
        Type::TypeArrow(t1, t2) => {
            let mut preds = predicates(*t1);
            preds.extend(predicates(*t2));
            preds
        }
        Type::TypeAbs(_, _, _) => todo!(),
        Type::TypeApp(_, _) => todo!(),
        Type::TypeAll(_, _, _) => todo!(),
        Type::Base(_) => vec![],
        Type::Reference(_) => todo!(),
        Type::Tuple(_) => todo!(),
        Type::Record(_) => todo!(),
        Type::Variants(_) => todo!(),
        Type::Recursive(_, _, _) => todo!(),
        Type::Existential(_, _, _) => todo!(),
        Type::Predicate(x, t, rest) => {
            let mut preds = vec![(x, *t)];
            preds.extend(predicates(*rest));
            preds
        }
    }
}

pub fn prefixed_predicates(t: Type) -> (Vec<(String, Type)>, Type) {
    let mut vars = vec![];
    let mut current = t;

    while let Type::Predicate(x, ty, inner) = current {
        vars.push((x, *ty));
        current = *inner;
    }

    (vars, current)
}

pub fn substitutions(t1: Type, t2: Type, allowed_substitutions: &Vec<String>) -> Vec<(String, Type)> {
    match (t1, t2) {
        (Type::TypeVar(x), t) if allowed_substitutions.contains(&x) => vec![(x, t)],
        (Type::TypeVar(x1), Type::TypeVar(x2)) => {
            if x1 != x2 {
                panic!("Could not provide a substitution since the type vars are different");
            }

            vec![]
        }
        (Type::Base(b1), Type::Base(b2)) if b1 == b2 => {
            vec![]
        }
        (Type::Tuple(v1), Type::Tuple(v2)) => {
            let mut subs = vec![];

            for (g1, g2) in v1.into_iter().zip(v2.into_iter()) {
                subs.extend(substitutions(g1, g2, allowed_substitutions))
            }

            subs
        }
        (Type::Record(h1), Type::Record(h2)) |
        (Type::Variants(h1), Type::Variants(h2)) => {
            let mut subs = vec![];

            for (k1, v1) in &h1 {
                if let Some(v2) = h2.get(k1) {
                    subs.extend(substitutions(v1.clone(), v2.clone(), allowed_substitutions))
                } else {
                    panic!("The fields does not match and are therefore not unifiable.")
                }
            }

            subs
        }
        (Type::TypeApp(t11, t12), Type::TypeApp(t21, t22)) |
        (Type::TypeArrow(t11, t12), Type::TypeArrow(t21, t22)) => {
            let mut subs = vec![];

            subs.extend(substitutions(*t11, *t21, allowed_substitutions));
            subs.extend(substitutions(*t12, *t22, allowed_substitutions));

            subs
        }
        (Type::Reference(t1), Type::Reference(t2)) => substitutions(*t1, *t2, allowed_substitutions),
        (Type::TypeAbs(x1, k1, t1), Type::TypeAbs(x2, k2, t2)) |
        (Type::Existential(x1, k1, t1), Type::Existential(x2, k2, t2)) |
        (Type::Recursive(x1, k1, t1), Type::Recursive(x2, k2, t2)) |
        (Type::TypeAll(x1, k1, t1), Type::TypeAll(x2, k2, t2)) => {
            assert_eq!(k1, k2);

            if x1 != x2 {
                panic!("The ForAll/Existential/Recursive labels are not equal")
            }

            let mut new_allowed = allowed_substitutions.iter().cloned().filter(|i| i != &x1).collect::<Vec<_>>();
            substitutions(*t1, *t2, &new_allowed)
        }
        (Type::Predicate(x1, t11, t12), Type::Predicate(x2, t21, t22)) => {
            if x1 != x2 {
                panic!("The predicate labels are not equal")
            }

            let mut subs = vec![];
            subs.extend(substitutions(*t11, *t21, allowed_substitutions));
            subs.extend(substitutions(*t12, *t22, allowed_substitutions));
            subs
        }
        (Type::Predicate(x1, t1, t2), t3) => {
            let mut subs = vec![];
            subs.extend(substitutions(*t1, t3.clone(), allowed_substitutions));
            subs.extend(substitutions(*t2, t3.clone(), allowed_substitutions));
            subs
        }
        (t1, t2) => panic!("The type {} are not equal to {} with the allowed_substitutions: {:?}", t1, t2, &allowed_substitutions)
    }
}

pub fn prefixed_for_all_vars(t: Type) -> (Vec<String>, Type) {
    let mut vars = vec![];
    let mut current = t;

    while let Type::TypeAll(x, _, inner) = current {
        vars.push(x);
        current = *inner;
    }

    (vars, current)
}

mod tests {
    use std::collections::{HashMap, HashSet};
    use crate::free_variables::{free_term_variables, free_type_variables, substitutions, prefixed_for_all_vars, type_specialization};
    use crate::{add_binding, BaseType, Binding, Context, Term, Type};
    use crate::Kind::KindStar;

    mod substitution_var_with_type {
        use std::collections::HashMap;
        use crate::{BaseType, Context, Type};
        use crate::free_variables::substitutions;
        use crate::Kind::KindStar;

        #[test]
        fn substitution_var_with_type() {
            // Arrange
            let t1 = Type::TypeVar("X".to_string());
            let t2 = Type::Base(BaseType::Int);

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec!["X".to_string()]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        fn substitution_var_with_type2() {
            // Arrange
            let t1 = Type::TypeVar("X".to_string());
            let t2 = Type::TypeArrow(
                Box::new(Type::Tuple(vec![Type::Base(BaseType::Int)])),
                Box::new(Type::Base(BaseType::Bool)),
            );

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec!["X".to_string()]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        #[should_panic]
        fn substitution_var_with_type3() {
            // Arrange
            let t1 = Type::TypeVar("X".to_string());
            let t2 = Type::TypeArrow(
                Box::new(Type::Tuple(vec![Type::Base(BaseType::Int)])),
                Box::new(Type::Base(BaseType::Bool)),
            );

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec![]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        fn substitution_var_with_type4() {
            // Arrange
            let t1 = Type::TypeVar("X".to_string());
            let t2 = Type::TypeVar("X".to_string());

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec![]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        fn substitution_var_with_type5() {
            // Arrange
            let t1 = Type::TypeVar("X".to_string());
            let t2 = Type::TypeVar("X".to_string());

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec!["X".to_string()]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        fn substitution_var_with_type6() {
            // Arrange
            let t1 = Type::Base(BaseType::Bool);
            let t2 = Type::Base(BaseType::Bool);

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec![]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        #[should_panic]
        fn substitution_var_with_type7() {
            // Arrange
            let t1 = Type::Base(BaseType::Int);
            let t2 = Type::Base(BaseType::Bool);

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec![]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        fn substitution_var_with_type8() {
            // Arrange
            let t1 = Type::Tuple(vec![Type::Base(BaseType::Bool), Type::Base(BaseType::Unit)]);
            let t2 = Type::Tuple(vec![Type::Base(BaseType::Bool), Type::Base(BaseType::Unit)]);

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec![]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        #[should_panic]
        fn substitution_var_with_type9() {
            // Arrange
            let t1 = Type::Tuple(vec![Type::Base(BaseType::Bool), Type::Base(BaseType::Unit)]);
            let t2 = Type::Tuple(vec![Type::Base(BaseType::Bool), Type::Base(BaseType::Int)]);

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec![]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        fn substitution_var_with_type10() {
            // Arrange
            let t1 = Type::Record(HashMap::from([("l1".to_string(), Type::Base(BaseType::Bool)), ("l2".to_string(), Type::Base(BaseType::Float))]));
            let t2 = Type::Record(HashMap::from([("l1".to_string(), Type::Base(BaseType::Bool)), ("l2".to_string(), Type::Base(BaseType::Float))]));

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec![]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        #[should_panic]
        fn substitution_var_with_type11() {
            // Arrange
            let t1 = Type::Record(HashMap::from([("l1".to_string(), Type::Base(BaseType::Bool)), ("l2".to_string(), Type::Base(BaseType::Float))]));
            let t2 = Type::Record(HashMap::from([("l3".to_string(), Type::Base(BaseType::Bool)), ("l2".to_string(), Type::Base(BaseType::Float))]));

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec![]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        #[should_panic]
        fn substitution_var_with_type12() {
            // Arrange
            let t1 = Type::Record(HashMap::from([("l1".to_string(), Type::Base(BaseType::Float)), ("l2".to_string(), Type::Base(BaseType::Float))]));
            let t2 = Type::Record(HashMap::from([("l1".to_string(), Type::Base(BaseType::Bool)), ("l2".to_string(), Type::Base(BaseType::Float))]));

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec![]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        fn substitution_var_with_type13() {
            // Arrange
            let t1 = Type::TypeArrow(
                Box::new(Type::Tuple(vec![Type::Base(BaseType::Int)])),
                Box::new(Type::Base(BaseType::Bool)),
            );
            let t2 = Type::TypeArrow(
                Box::new(Type::Tuple(vec![Type::Base(BaseType::Int)])),
                Box::new(Type::Base(BaseType::Bool)),
            );

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec![]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        #[should_panic]
        fn substitution_var_with_type14() {
            // Arrange
            let t1 = Type::TypeArrow(
                Box::new(Type::Tuple(vec![Type::Base(BaseType::Int)])),
                Box::new(Type::Base(BaseType::Bool)),
            );
            let t2 = Type::TypeArrow(
                Box::new(Type::Tuple(vec![Type::Base(BaseType::Int)])),
                Box::new(Type::Base(BaseType::Float)),
            );

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec![]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        fn substitution_var_with_type15() {
            // Arrange
            let t1 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::TypeVar("X".to_string())));
            let t2 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::TypeVar("X".to_string())));

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec!["X".to_string()]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }

        #[test]
        #[should_panic]
        fn substitution_var_with_type16() {
            // Arrange
            let t1 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::TypeVar("X".to_string())));
            let t2 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::TypeVar("Y".to_string())));

            println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            println!("Substitute: {:?}", substitutions(t1, t2, &vec!["X".to_string()]));

            // Assert
            //assert_eq!(vars, HashSet::from([]))
        }
    }

    #[test]
    fn prefixed_forall_vars() {
        // Arrange
        let t1 = Type::TypeAll("X".to_string(), KindStar, Box::new(
            Type::TypeAll("Y".to_string(), KindStar, Box::new(Type::Base(BaseType::Int)))
        ));

        println!("t1 = {}", t1.to_string_type(&Context::new(), 0));

        // Act
        println!("{:?}", prefixed_for_all_vars(t1))

        // Assert
        //assert_eq!(vars, HashSet::from([]))
    }

    #[test]
    fn specialization() {
        // Arrange
        let t1 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::Base(BaseType::Int)));
        let t2 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::Base(BaseType::Int)));

        println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

        // Act
        let is_specialization = type_specialization(t1, t2, &Context::new());

        // Assert
        //assert_eq!(vars, HashSet::from([]))
    }

    #[test]
    #[should_panic]
    fn specialization2() {
        // Arrange
        let t1 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::TypeArrow(
            Box::new(Type::TypeVar("X".to_string())),
            Box::new(Type::TypeVar("X".to_string())),
        )));
        let t2 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::TypeArrow(
            Box::new(Type::Base(BaseType::Float)),
            Box::new(Type::Base(BaseType::Int)),
        )));

        println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

        // Act
        let is_specialization = type_specialization(t1, t2, &Context::new());

        // Assert
        //assert_eq!(vars, HashSet::from([]))
    }

    #[test]
    fn specialization3() {
        // Arrange
        let t1 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::TypeArrow(
            Box::new(Type::TypeVar("X".to_string())),
            Box::new(Type::TypeVar("X".to_string())),
        )));
        let t2 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::TypeArrow(
            Box::new(Type::Base(BaseType::Float)),
            Box::new(Type::Base(BaseType::Float)),
        )));

        println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

        // Act
        let is_specialization = type_specialization(t1, t2, &Context::new());

        // Assert
        //assert_eq!(vars, HashSet::from([]))
    }

    #[test]
    fn specialization4() {
        // Arrange
        let t1 = Type::Predicate("eq".to_string(), Box::new(
            Type::TypeArrow(
                Box::new(Type::Base(BaseType::Int)),
                Box::new(Type::TypeArrow(
                    Box::new(Type::Base(BaseType::Int)),
                    Box::new(Type::Base(BaseType::Bool)),
                ))
            )
        ), Box::new(Type::Base(BaseType::Unit)));

        let t2 = Type::Predicate("eq".to_string(), Box::new(
            Type::TypeArrow(
                Box::new(Type::Base(BaseType::Int)),
                Box::new(Type::TypeArrow(
                    Box::new(Type::Base(BaseType::Int)),
                    Box::new(Type::Base(BaseType::Bool)),
                ))
            )
        ), Box::new(Type::Base(BaseType::Unit)));

        println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

        // Act
        let is_specialization = type_specialization(t1, t2, &Context::new());

        // Assert
        //assert_eq!(vars, HashSet::from([]))
    }

    #[test]
    #[should_panic]
    fn specialization5_without_context() {
        // Arrange
        let t1 = Type::TypeAll("X".to_string(), KindStar, Box::new(
            Type::Predicate("eq".to_string(), Box::new(
                Type::TypeArrow(
                    Box::new(Type::TypeVar("X".to_string())),
                    Box::new(Type::TypeArrow(
                        Box::new(Type::TypeVar("X".to_string())),
                        Box::new(Type::Base(BaseType::Bool)),
                    ))
                )
            ), Box::new(
                Type::TypeArrow(
                    Box::new(Type::TypeVar("X".to_string())),
                    Box::new(Type::TypeArrow(
                        Box::new(Type::TypeVar("X".to_string())),
                        Box::new(Type::Base(BaseType::Bool)),
                    ))
                )
            ))
        ));

        let t2 = Type::TypeArrow(
            Box::new(Type::Base(BaseType::Int)),
            Box::new(Type::TypeArrow(
                Box::new(Type::Base(BaseType::Int)),
                Box::new(Type::Base(BaseType::Bool)),
            ))
        );

        println!("Checking: {} ⊑ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

        // Act
        let is_specialization = type_specialization(t1, t2, &Context::new());

        // Assert
        //assert_eq!(vars, HashSet::from([]))
    }

    #[test]
    fn specialization5_with_context() {
        // Arrange
        let t1 = Type::TypeAll("X".to_string(), KindStar, Box::new(
            Type::Predicate("eq".to_string(), Box::new(
                Type::TypeArrow(
                    Box::new(Type::TypeVar("X".to_string())),
                    Box::new(Type::TypeArrow(
                        Box::new(Type::TypeVar("X".to_string())),
                        Box::new(Type::Base(BaseType::Bool)),
                    ))
                )
            ), Box::new(
                Type::TypeArrow(
                    Box::new(Type::TypeVar("X".to_string())),
                    Box::new(Type::TypeArrow(
                        Box::new(Type::TypeVar("X".to_string())),
                        Box::new(Type::Base(BaseType::Bool)),
                    ))
                )
            ))
        ));

        let t2 = Type::TypeArrow(
            Box::new(Type::Base(BaseType::Int)),
            Box::new(Type::TypeArrow(
                Box::new(Type::Base(BaseType::Int)),
                Box::new(Type::Base(BaseType::Bool)),
            ))
        );

        let context = Context::new();

        let context = add_binding(&context, Binding::OverBind("eq".to_string(), Type::TypeAll(
            "X".to_string(),
            KindStar,
            Box::new(Type::TypeArrow(
                Box::new(Type::TypeVar("X".to_string())),
                Box::new(Type::TypeArrow(
                    Box::new(Type::TypeVar("X".to_string())),
                    Box::new(Type::Base(BaseType::Bool)),
                ))
            ))
        )));

        let context = add_binding(&context, Binding::InstBind("eq".to_string(), Type::TypeArrow(
            Box::new(Type::Base(BaseType::Int)),
            Box::new(Type::TypeArrow(
                Box::new(Type::Base(BaseType::Int)),
                Box::new(Type::Base(BaseType::Bool)),
            ))
        )));

        println!("Checking: {} ⊑ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));

        // Act
        let is_specialization = type_specialization(t1, t2, &context);

        // Assert
        //assert_eq!(vars, HashSet::from([]))
    }

    #[test]
    #[should_panic]
    fn specialization6_not_in_context() {
        // Arrange
        let t1 = Type::TypeAll("X".to_string(), KindStar, Box::new(
            Type::Predicate("eq".to_string(), Box::new(
                Type::TypeArrow(
                    Box::new(Type::TypeVar("X".to_string())),
                    Box::new(Type::TypeArrow(
                        Box::new(Type::TypeVar("X".to_string())),
                        Box::new(Type::Base(BaseType::Bool)),
                    ))
                )
            ), Box::new(
                Type::TypeArrow(
                    Box::new(Type::TypeVar("X".to_string())),
                    Box::new(Type::TypeArrow(
                        Box::new(Type::TypeVar("X".to_string())),
                        Box::new(Type::Base(BaseType::Bool)),
                    ))
                )
            ))
        ));

        let t2 = Type::TypeArrow(
            Box::new(Type::Base(BaseType::Float)),
            Box::new(Type::TypeArrow(
                Box::new(Type::Base(BaseType::Float)),
                Box::new(Type::Base(BaseType::Bool)),
            ))
        );

        let context = Context::new();

        let context = add_binding(&context, Binding::OverBind("eq".to_string(), Type::TypeAll(
            "X".to_string(),
            KindStar,
            Box::new(Type::TypeArrow(
                Box::new(Type::TypeVar("X".to_string())),
                Box::new(Type::TypeArrow(
                    Box::new(Type::TypeVar("X".to_string())),
                    Box::new(Type::Base(BaseType::Bool)),
                ))
            ))
        )));

        let context = add_binding(&context, Binding::InstBind("eq".to_string(), Type::TypeArrow(
            Box::new(Type::Base(BaseType::Int)),
            Box::new(Type::TypeArrow(
                Box::new(Type::Base(BaseType::Int)),
                Box::new(Type::Base(BaseType::Bool)),
            ))
        )));

        println!("Checking: {} ⊑ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));

        // Act
        let is_specialization = type_specialization(t1, t2, &context);

        // Assert
        //assert_eq!(vars, HashSet::from([]))
    }


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
