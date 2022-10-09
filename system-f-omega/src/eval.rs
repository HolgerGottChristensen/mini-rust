use std::collections::HashMap;
use crate::{add_binding, add_name, BaseType, Binding, Context, get_binding, get_kind, get_type, Kind, Term, Type, type_substitution};
use crate::Type::TypeArrow;

pub fn is_val(context: &Context, term: Term) -> bool {
    match term {
        Term::TermAbs(_, _, _) => true,
        Term::TermTypeAbs(_, _, _) => true,
        _ => false
    }
}

/*
pub fn eval1(context: &Context, term: Term) -> Result<Term, Term> {
    match term {
        Term::TermApp(t1, t2) => {
            match *t1 {
                Term::TermAbs(x, tyT11, t12) if is_val(context, *t2.clone()) => {
                    return Ok(term_substitution_top(*t2, *t12))
                }
                _ => ()
            }

            if is_val(context, *t1.clone()) {
                match eval1(context, *t2) {
                    Ok(t2_new) => Ok(Term::TermApp(t1, Box::new(t2_new))),
                    Err(t2_new) => Err(Term::TermApp(t1, Box::new(t2_new)))
                }
            } else {
                match eval1(context, *t1) {
                    Ok(t1_new) => Ok(Term::TermApp(Box::new(t1_new), t2)),
                    Err(t1_new) => Err(Term::TermApp(Box::new(t1_new), t2))
                }
            }
        }
        Term::TermTypeApp(t1, tyT2) => {
            match *t1 {
                Term::TermTypeAbs(x,_,t11) => {
                    return Ok(type_term_substitution_top(tyT2, *t11))
                }
                _ => ()
            }

            match eval1(context, *t1) {
                Ok(t1_new) => Ok(Term::TermTypeApp(Box::new(t1_new), tyT2)),
                Err(t1_new) => Err(Term::TermTypeApp(Box::new(t1_new), tyT2))
            }
        }
        _ => Err(term)
    }
}

pub fn eval(context: &Context, term: Term) -> Term {
    match eval1(context, term) {
        Ok(t_new) => {
            eval(context, t_new)
        }
        Err(t) => t,
    }
}
*/

// KINDING

pub fn compute_type(context: &Context, tyt: Type) -> Result<Type, Type> {
    match tyt.clone() {
        // T-AppAbs
        Type::TypeApp(tyT1, tyT2) => {
            match *tyT1 {
                Type::TypeAbs(name, _, tyT12) => {
                    return Ok(type_substitution(&name, *tyT2, *tyT12))
                }
                _ => ()
            }
        }
        _ => ()
    }

    Err(tyt)
}

pub fn simplify_type(context: &Context, tyt: Type) -> Type {
    let tyt_new = match tyt {
        Type::TypeApp(tyT1,tyT2) =>
            Type::TypeApp(Box::new(simplify_type(context, *tyT1)), tyT2),
        t => t,
    };

    match compute_type(context, tyt_new) {
        Ok(t) => simplify_type(context, t),
        Err(t) => t,
    }
}

pub fn type_equivalence(context: &Context, tys: Type, tyt: Type) -> bool {
    let simple_tys = simplify_type(context, tys);
    let simple_tyt = simplify_type(context, tyt);

    match (simple_tys, simple_tyt) {
        (Type::TypeArrow(s1, s2), Type::TypeArrow(t1, t2)) => {
            type_equivalence(context, *s1, *t1) && type_equivalence(context, *s2, *t2)
        }
        (Type::TypeVar(i), Type::TypeVar(j)) => i == j, // Todo: Maybe this is not the correct equivalence?
        (Type::TypeAbs(tyX1,knKS1,tyS2), Type::TypeAbs(_,knKT1,tyT2)) => {
            let new_context = add_name(context, tyX1);

            knKS1 == knKT1 && type_equivalence(&new_context, *tyS2, *tyT2)
        }
        (Type::TypeApp(tyS1,tyS2), Type::TypeApp(tyT1,tyT2)) => {
            type_equivalence(context, *tyS1, *tyT1) && type_equivalence(context, *tyS2, *tyT2)
        }
        (Type::Base(b1), Type::Base(b2)) => b1 == b2,

        (Type::TypeAll(tyX1,knK1,tyS2), Type::TypeAll(_,knK2,tyT2)) |
        (Type::Existential(tyX1,knK1,tyS2), Type::Existential(_,knK2,tyT2)) |
        (Type::Recursive(tyX1,knK1,tyS2), Type::Recursive(_,knK2,tyT2)) => {
            let new_context = add_name(context, tyX1);

            knK1 == knK2 && type_equivalence(&new_context, *tyS2, *tyT2)
        }

        (Type::Reference(tyS1), Type::Reference(tyT1)) => {
            type_equivalence(context, *tyS1, *tyT1)
        }
        (Type::Tuple(tyS1), Type::Tuple(tyT1)) => {
            if tyS1.len() == tyT1.len() {
                for (ty1, ty2) in tyS1.iter().cloned().zip(tyT1.iter().cloned()) {
                    if !type_equivalence(context, ty1, ty2) {
                        return false;
                    }
                }

                true
            } else {
                false
            }
        }
        (Type::Variants(tyS1), Type::Variants(tyT1)) |
        (Type::Record(tyS1), Type::Record(tyT1)) => {
            if tyS1.len() == tyT1.len() {
                for (label, ty1) in tyS1 {
                    match tyT1.get(&label).cloned() {
                        None => {
                            return false;
                        },
                        Some(ty2) => {
                            if !type_equivalence(context, ty1, ty2) {
                                return false;
                            }
                        }
                    }
                }

                true
            } else {
                false
            }
        }
        _ => false
    }
}

/// Get the Kind of a Type
pub fn kind_of(context: &Context, t: Type) -> Kind {
    // Match on the type
    match t {
        // K-Arrow
        Type::TypeArrow(t1, t2) => {
            if kind_of(context, *t1) != Kind::KindStar {
                panic!("Star kind expected")
            }
            if kind_of(context, *t2) != Kind::KindStar {
                panic!("Star kind expected")
            }

            Kind::KindStar
        }
        Type::TypeVar(i) => {
            get_kind(context, &i)
        }
        Type::TypeAbs(x, k1, k2) => {
            let new_context = add_binding(context, Binding::TyVarBinding(x, k1.clone()));

            let k2 = kind_of(&new_context, *k2);
            Kind::KindArrow(Box::new(k1), Box::new(k2))
        }
        Type::TypeApp(t1, t2) => {
            let k1 = kind_of(context, *t1);
            let k2 = kind_of(context, *t2);

            match k1 {
                Kind::KindArrow(k11, k12) => {
                    if k2 == *k11 {
                        *k12
                    } else {
                        panic!("parameter kind mismatch")
                    }
                }
                _ => panic!("Expected arrow kind")
            }
        }
        Type::TypeAll(x, k1, t2) => {
            let new_context = add_binding(context, Binding::TyVarBinding(x, k1.clone()));

            if kind_of(&new_context, *t2) != Kind::KindStar {
                panic!("Kind * expected")
            }

            Kind::KindStar
        }
        Type::Base(_) => Kind::KindStar,
        Type::Recursive(x, k1, t2) => {
            let new_context = add_binding(context, Binding::TyVarBinding(x, k1.clone()));

            if kind_of(&new_context, *t2) != Kind::KindStar {
                panic!("Kind * expected")
            }

            Kind::KindStar
        }
        Type::Reference(t1) => {
            // Todo: Is this actually correct?
            if kind_of(context, *t1) != Kind::KindStar {
                panic!("Star kind expected")
            }

            Kind::KindStar
        },
        Type::Tuple(types) => {
            for ty in types {
                // Todo: Is this actually correct?
                if kind_of(context, ty) != Kind::KindStar {
                    panic!("Star kind expected")
                }
            }

            Kind::KindStar
        },
        Type::Variants(types) |
        Type::Record(types) => {
            for (_, ty) in types {
                // Todo: Is this actually correct?
                if kind_of(context, ty) != Kind::KindStar {
                    panic!("Star kind expected")
                }
            }

            Kind::KindStar
        }
        Type::Existential(x, k1, t2) => {
            let new_context = add_binding(context, Binding::TyVarBinding(x, k1.clone()));

            if kind_of(&new_context, *t2) != Kind::KindStar {
                panic!("Kind * expected")
            }

            Kind::KindStar
        }
    }
}

pub fn check_kind_star(context: &Context, tyt: Type) {
    if kind_of(context, tyt) != Kind::KindStar {
        panic!("Kind * expected")
    }
}

// TYPING

pub fn type_of(context: &Context, term: Term) -> Type {
    match term {
        // T-Var
        Term::TermVar(name) => {
            get_type(context, &name)
        },
        // T-Abs
        Term::TermAbs(x, t1, term2) => {
            check_kind_star(context, t1.clone());

            let new_context = add_binding(context, Binding::VarBinding(x, t1.clone()));

            let t2 = type_of(&new_context, *term2);
            Type::TypeArrow(Box::new(t1), Box::new(t2))
        }
        // T-App
        Term::TermApp(term1, term2) => {
            let t1 = type_of(context, *term1);
            let t2 = type_of(context, *term2);

            match simplify_type(context, t1) {
                Type::TypeArrow(t11, t12) => {
                    if type_equivalence(context, t2, *t11) {
                        *t12
                    } else {
                        panic!("parameter type mismatch")
                    }
                }
                _ => panic!("arrow type expected")
            }
        }
        // T-TAbs
        Term::TermTypeAbs(x, k1, term2) => {
            let new_context = add_binding(context, Binding::TyVarBinding(x.clone(), k1.clone()));

            let t2 = type_of(&new_context, *term2);

            Type::TypeAll(x, k1, Box::new(t2))
        }
        // T-TApp
        Term::TermTypeApp(term1, t2) => {
            let k2 = kind_of(context, t2.clone());
            let t1 = type_of(context, *term1);
            match simplify_type(context, t1) {
                Type::TypeAll(name, k11, t12) => {
                    if k11 != k2 {
                        panic!("Type argument has wrong kind")
                    } else {
                        type_substitution(&name, t2, *t12)
                    }
                }
                _ => panic!("universal type expected")
            }
        }
        // T-If
        Term::If(term1, term2, term3) => {
            let t1 = type_of(context, *term1);
            let t1 = simplify_type(context, t1);

            if !type_equivalence(context, t1, Type::Base(BaseType::Bool)) {
                panic!("The type of the guard needs to be a Bool");
            }

            let t2 = type_of(context, *term2);
            let t2 = simplify_type(context, t2);

            let t3 = type_of(context, *term3);
            let t3 = simplify_type(context, t3);

            if !type_equivalence(context, t2.clone(), t3) {
                panic!("The type of the terms of the branches do not match");
            }

            t2
        }
        // T-True, T-False
        Term::True | Term::False => Type::Base(BaseType::Bool),
        // T-Integer
        Term::Integer(_) => Type::Base(BaseType::Int),
        // T-Float
        Term::Float(_) => Type::Base(BaseType::Float),
        // T-Unit
        Term::Unit => Type::Base(BaseType::Unit),
        Term::Reference(term) => {
            let t1 = type_of(context, *term);
            let t1 = simplify_type(context, t1);

            Type::Reference(Box::new(t1))
        }
        // T-Let
        Term::Let(x, term1, term2) => {
            let t1 = type_of(context, *term1);
            let t1 = simplify_type(context, t1);

            let new_context = add_binding(context, Binding::VarBinding(x, t1));

            let t2 = type_of(&new_context, *term2);

            t2
        }
        // T-Tuple
        Term::Tuple(terms) => {
            let mut types = Vec::new();

            for term in terms {
                let t = type_of(context, term);
                let t = simplify_type(context, t);
                types.push(t);
            }

            Type::Tuple(types)
        }
        // T-Proj
        Term::TupleProjection(term, index) => {
            match simplify_type(context, type_of(context, *term)) {
                Type::Tuple(types) => {
                    if index >= types.len() {
                        panic!("Trying to project a tuple with an index out of range")
                    }
                    types[index].clone()
                }
                _ => panic!("Only Tuples can be projected with indexes")
            }
        }
        // T-Rcd
        Term::Record(terms) => {
            let mut types = HashMap::new();
            for (label, term) in terms {
                let t = simplify_type(context, type_of(context, term));
                types.insert(label, t);
            }

            Type::Record(types)
        }
        // T-Proj
        Term::RecordProjection(term, label) => {
            match simplify_type(context, type_of(context, *term)) {
                Type::Record(types) => {
                    match types.get(&label) {
                        None => panic!("Projected with a label not in the record"),
                        Some(t) => t.clone(),
                    }
                }
                _ => panic!("Only records can be projected with labels")
            }
        }
        // T-Variant
        Term::Tagging(tag, term, t1) => {
            let t = simplify_type(context, type_of(context, *term));

            match simplify_type(context, t1.clone()) {
                Type::Variants(variants) => {
                    match variants.get(&tag).cloned() {
                        Some(case) => {
                            if type_equivalence(context, case, t) {
                                t1
                            } else {
                                panic!("The type does not match the type specified by the variant with the same label")
                            }
                        }
                        None => panic!("The label used is not a member of the variant")
                    }
                }
                _ => panic!("A tagging need to be tagged with a variant type, or something that simplifies to a variant type")
            }
        }
        // T-Case
        Term::Case(term, cases) => {
            match simplify_type(context, type_of(context, *term)) {
                Type::Variants(types) => {
                    let mut case_types = Vec::new();
                    for (label, (binding, inner_term)) in cases {
                        match types.get(&label) {
                            Some(ty) => {
                                let new_context = add_binding(context, Binding::VarBinding(binding, ty.clone()));

                                let inner_term_type = simplify_type(&new_context, type_of(&new_context, inner_term));
                                case_types.push(inner_term_type);
                            }
                            None => panic!("The case label is not specified in the type of the term, and can therefore not match")
                        }
                    }

                    if let Some(first) = case_types.first() {
                        let equal = case_types.iter().cloned().all(|a| type_equivalence(context, first.clone(), a));
                        if equal {
                            first.clone()
                        } else {
                            panic!("Not all cases in the case term return the same type as required")
                        }
                    } else {
                        panic!("The case term needs at least one case to type check")
                    }
                }
                _ => panic!("You can only use a case on a term that is a variant.")
            }
        }
        // T-Fold
        Term::Fold(ty) => {
            match simplify_type(context, ty.clone()) {
                Type::Recursive(label, kn, ty2) => {
                    TypeArrow(Box::new(type_substitution(&label, ty.clone(), *ty2)), Box::new(ty))
                }
                _ => panic!("Only recursive types can be folded")
            }
        }
        // T-UnFold
        Term::UnFold(ty) => {
            match simplify_type(context, ty.clone()) {
                Type::Recursive(label, kn, ty2) => {
                    TypeArrow(Box::new(ty.clone()), Box::new(type_substitution(&label,  ty, *ty2)))
                }
                _ => panic!("Only recursive types can be folded")
            }
        }
        // T-Pack
        Term::Pack(tyT1,t2,tyT) => {
            check_kind_star(context, tyT.clone());
            match simplify_type(context, tyT.clone()) {
                Type::Existential(tyY,k,tyT2) => {
                    if kind_of(context, tyT1.clone()) != k {
                        panic!("The type component does not have the correct kind")
                    }

                    let tyU = type_of(context, *t2);
                    let tyU_new = type_substitution(&tyY, tyT1, *tyT2);

                    if !type_equivalence(context, tyU, tyU_new) {
                        panic!("The expected existential type does not match the type of the term")
                    }

                    tyT
                }
                _ => panic!("You can only pack existential types")
            }

        }
        // T-UnPack
        Term::UnPack(tyX,x,t1,t2) => {
            let tyT1 = type_of(context, *t1);
            match simplify_type(context, tyT1) {
                Type::Existential(tyY,k,tyT11) => {
                    let context_new = add_binding(context, Binding::TyVarBinding(tyX, k));
                    let context_new = add_binding(&context_new, Binding::VarBinding(x, *tyT11));

                    type_of(&context_new, *t2)
                }
                _ => panic!("You can only pack existential types")
            }
        }
    }
}


