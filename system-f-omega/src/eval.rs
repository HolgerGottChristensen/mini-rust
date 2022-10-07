use crate::{add_binding, add_name, Binding, Context, get_binding, get_type_from_context, Kind, Term, term_substitution_top, Type, type_shift, type_substitution, type_substitution_top, type_term_substitution_top};
use crate::Type::{TypeAll, TypeArrow};

pub fn is_val(context: &Context, term: Term) -> bool {
    match term {
        Term::TermAbs(_, _, _) => true,
        Term::TermTypeAbs(_, _, _) => true,
        _ => false
    }
}

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


// KINDING

pub fn compute_type(context: &Context, tyt: Type) -> Result<Type, Type> {
    match tyt.clone() {
        Type::TypeApp(tyT1, tyT2) => {
            match *tyT1 {
                Type::TypeAbs(_, _, tyT12) => {
                    return Ok(type_substitution_top(*tyT2, *tyT12))
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
        (Type::TypeArrow(tyS1,tyS2), Type::TypeArrow(tyT1,tyT2)) => {
            type_equivalence(context, *tyS1, *tyT1) && type_equivalence(context, *tyS2, *tyT2)
        }
        (Type::TypeVar(i, _), Type::TypeVar(j, _)) => i == j,
        (Type::TypeAbs(tyX1,knKS1,tyS2), Type::TypeAbs(_,knKT1,tyT2)) => {
            let new_context = add_name(context, tyX1);

            knKS1 == knKT1 && type_equivalence(&new_context, *tyS2, *tyT2)
        }
        (Type::TypeApp(tyS1,tyS2), Type::TypeApp(tyT1,tyT2)) => {
            type_equivalence(context, *tyS1, *tyT1) && type_equivalence(context, *tyS2, *tyT2)
        }
        (Type::TypeAll(tyX1,knK1,tyS2), Type::TypeAll(_,knK2,tyT2)) => {
            let new_context = add_name(context, tyX1);

            knK1 == knK2 && type_equivalence(&new_context, *tyS2, *tyT2)
        }
        _ => false
    }
}

pub fn get_kind(context: &Context, i: i64) -> Kind {
    match get_binding(context, i) {
        Binding::TyVarBinding(kNk) => kNk,
        _ => panic!("Binding at position {} is not a TyVarBinding: {:?}", i, context)
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
        Type::TypeVar(i, _) => {
            get_kind(context, i)
        }
        Type::TypeAbs(x, k1, k2) => {
            let new_context = add_binding(context, x, Binding::TyVarBinding(k1.clone()));

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
            let new_context = add_binding(context, x, Binding::TyVarBinding(k1.clone()));

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
        Term::TermVar(i,_) => {
            get_type_from_context(context, i)
        },
        // T-Abs
        Term::TermAbs(x, t1, term2) => {
            check_kind_star(context, t1.clone());

            let new_context = add_binding(context, x, Binding::VarBinding(t1.clone()));

            let t2 = type_of(&new_context, *term2);
            //println!("{:?}", &tyt2);
            //println!("{:?}", &context);
            //println!("{:?}", &new_context);

            dbg!(&t2);
            let t2 = type_shift(-1, t2);
            //let t2 = type_shift(-1, t2);
            dbg!(&t2);

            TypeArrow(Box::new(t1), Box::new(t2)) // Original
            //TypeArrow(Box::new(tyT1), Box::new(type_shift(0, tyt2)))
        }
        // T-App
        Term::TermApp(t1,t2) => {
            let tyT1 = type_of(context, *t1);
            let tyT2 = type_of(context, *t2);

            match simplify_type(context, tyT1) {
                Type::TypeArrow(tyT11,tyT12) => {
                    if type_equivalence(context, tyT2, *tyT11) {
                        *tyT12
                    } else {
                        panic!("parameter type mismatch")
                    }
                }
                _ => panic!("arrow type expected")
            }
        }
        // T-TAbs
        Term::TermTypeAbs(tyX,knK1,t2) => {
            let new_context = add_binding(context, tyX.clone(), Binding::TyVarBinding(knK1.clone()));

            let tyT2 = type_of(&new_context, *t2);

            TypeAll(tyX, knK1, Box::new(tyT2))
        }
        // T-TApp
        Term::TermTypeApp(t1,tyT2) => {
            let knKT2 = kind_of(context, tyT2.clone());
            let tyT1 = type_of(context, *t1);
            match simplify_type(context, tyT1) {
                TypeAll(_,knK11,tyT12) => {
                    if knK11 != knKT2 {
                        panic!("Type argument has wrong kind")
                    } else {
                        type_substitution_top(tyT2, *tyT12)
                    }
                }
                _ => panic!("universal type expected")
            }
        }
    }
}


