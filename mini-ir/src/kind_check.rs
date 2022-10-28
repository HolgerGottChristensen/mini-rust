use crate::{Context, Term};
use crate::binding::Binding;
use crate::kind::Kind;
use crate::types::Type;

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
            context.get_kind(&i).unwrap()
        }
        Type::TypeAbs(x, k1, k2) => {
            let new_context = context.add_binding(Binding::TyVarBinding(x, k1.clone()));

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
            let new_context = context.add_binding(Binding::TyVarBinding(x, k1.clone()));

            if kind_of(&new_context, *t2) != Kind::KindStar {
                panic!("Kind * expected")
            }

            Kind::KindStar
        }
        Type::Base(_) => Kind::KindStar,
        Type::Recursive(x, k1, t2) => {
            let new_context = context.add_binding(Binding::TyVarBinding(x, k1.clone()));

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
            let new_context = context.add_binding(Binding::TyVarBinding(x, k1.clone()));

            if kind_of(&new_context, *t2) != Kind::KindStar {
                panic!("Kind * expected")
            }

            Kind::KindStar
        }
        Type::Qualified(_, ty) => {
            kind_of(context, *ty)
        }
    }
}

pub fn check_kind_star(context: &Context, tyt: Type) {
    if kind_of(context, tyt) != Kind::KindStar {
        panic!("Kind * expected")
    }
}



