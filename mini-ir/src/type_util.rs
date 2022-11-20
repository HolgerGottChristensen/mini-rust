use std::collections::HashMap;

use crate::Context;
use crate::types::Type;
use crate::types::Type::TypeVar;

pub fn compute_type(context: &Context, tyt: Type) -> Result<Type, Type> {
    match tyt.clone() {
        // T-AppAbs
        Type::TypeApp(tyT1, tyT2) => {
            match *tyT1 {
                Type::TypeAbs(name, _, tyT12) => {
                    return Ok(type_substitution(&name, *tyT2, *tyT12));
                }
                TypeVar(name) => {
                    match context.get_type(&name) {
                        Ok(t) => return compute_type(context, Type::TypeApp(Box::new(t), tyT2)),
                        Err(_) => return Err(Type::TypeApp(Box::new(Type::TypeVar(name)), tyT2))
                    }
                }
                _ => ()
            }
        }
        TypeVar(x) => return context.get_type(&x).map_err(|_| TypeVar(x)),
        _ => ()
    }

    Err(tyt)
}

pub fn simplify_type(context: &Context, tyt: Type) -> Type {
    let tyt_new = match tyt {
        Type::TypeApp(tyT1, tyT2) =>
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

    //println!("Test equivalence between: {} and {} with context: {:?}", simple_tyt.to_string_type(context, 0), simple_tys.to_string_type(context, 0), context);

    match (simple_tys, simple_tyt) {
        (Type::TypeArrow(s1, s2), Type::TypeArrow(t1, t2)) => {
            type_equivalence(context, *s1, *t1) && type_equivalence(context, *s2, *t2)
        }
        (TypeVar(i), TypeVar(j)) => {
            match (context.get_type(&i), context.get_type(&j)) {
                (Ok(ti), Ok(tj)) => type_equivalence(context, ti, tj),
                (Ok(ti), Err(_)) => type_equivalence(context, ti, TypeVar(j)),
                (Err(_), Ok(tj)) => type_equivalence(context, TypeVar(i), tj),
                (Err(_), Err(_)) => i == j, // Todo: This is only correct if all binding names are unique
            }
        }
        (ty, TypeVar(i)) | (TypeVar(i), ty) => {
            println!("{}", context);
            println!("{:?}", ty);
            println!("{:?}", i);
            // Todo: How do we handle KindVars?
            context.get_type(&i).map(|a| type_equivalence(context, a, ty)).unwrap_or(false)
        }
        (Type::TypeAbs(tyX1, knKS1, tyS2), Type::TypeAbs(_, knKT1, tyT2)) => {
            let new_context = context.add_name(tyX1);

            knKS1 == knKT1 && type_equivalence(&new_context, *tyS2, *tyT2)
        }
        (Type::TypeApp(tyS1, tyS2), Type::TypeApp(tyT1, tyT2)) => {
            type_equivalence(context, *tyS1, *tyT1) && type_equivalence(context, *tyS2, *tyT2)
        }
        (Type::Base(b1), Type::Base(b2)) => b1 == b2,

        (Type::TypeAll(tyX1, knK1, tyS2), Type::TypeAll(_, knK2, tyT2)) => {
            let new_context = context.add_name(tyX1);

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
                        }
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

pub fn type_map(on_var: &dyn Fn(String) -> Type, ty: Type) -> Type {
    match ty {
        Type::TypeVar(s) => on_var(s),
        Type::TypeArrow(ty1, ty2) => Type::TypeArrow(
            Box::new(type_map(on_var, *ty1)),
            Box::new(type_map(on_var, *ty2)),
        ),
        Type::TypeAbs(tyX, knK1, tyT2) =>
            Type::TypeAbs(tyX, knK1, Box::new(type_map(on_var, *tyT2))),
        Type::TypeApp(tyT1, tyT2) => Type::TypeApp(
            Box::new(type_map(on_var, *tyT1)),
            Box::new(type_map(on_var, *tyT2)),
        ),
        Type::TypeAll(tyX, knK1, tyT2) => Type::TypeAll(
            tyX,
            knK1,
            Box::new(type_map(on_var, *tyT2)),
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

    type_map(&on_var, original)
}