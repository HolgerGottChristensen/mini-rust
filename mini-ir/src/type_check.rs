use std::collections::HashMap;
use paris::log;

use crate::{check_kind_star, Context, Kind, kind_of, Term};
use crate::base_type::BaseType;
use crate::binding::Binding;
use crate::Type::TypeAll;
use crate::type_util::{simplify_type, type_equivalence, type_substitution};
use crate::types::Type;
use crate::types::Type::{TypeArrow, TypeVar};

/// Check the type of a given term with the context. We need all classes to be the first,
/// and instances to be the second.
pub fn type_of(context: &Context, term: Term) -> Result<Type, String> {
    //log!("Check type of:\n\t{:?},\n\tsubs: {:?},\n\tcontext: {}", term, substitutions, context);
    match term {
        // T-Var
        Term::TermVar(name) => {
            context.get_type(&name)
        }
        // T-Abs
        Term::TermAbs(x, t1, term2) => {
            //check_kind_star(context, t1.clone()); todo: Re-add

            let mut new_context = context.add_binding(Binding::VarBinding(x, t1.clone()));

            let t2 = type_of(&mut new_context, *term2)?;
            Ok(TypeArrow(Box::new(t1), Box::new(t2)))
        }
        // T-App
        Term::TermApp(term1, term2) => {
            let t1 = type_of(context, *term1)?;
            let t2 = type_of(context, *term2)?;

            match simplify_type(context, t1) {
                TypeArrow(t11, t12) => {
                    if type_equivalence(context, t2.clone(), *t11.clone()) {
                        Ok(*t12)
                    } else {
                        Err(format!(
                            "Cannot apply: {}, to function of type: {}",
                            t2.to_string_type(context, 0),
                            TypeArrow(t11, t12).to_string_type(context, 0)
                        ))
                    }
                }
                _ => Err("arrow type expected".to_string())
            }
        }
        // T-TAbs
        Term::TermTypeAbs(x, k1, term2) => {
            let mut new_context = context.add_binding(Binding::TyVarBinding(x.clone(), k1.clone()));

            let t2 = type_of(&mut new_context, *term2)?;

            Ok(Type::TypeAll(x, k1, Box::new(t2)))
        }
        // T-TApp
        Term::TermTypeApp(term1, t2) => {
            let k2 = kind_of(context, t2.clone())?;
            let t1 = type_of(context, *term1)?;
            match simplify_type(context, t1) {
                Type::TypeAll(name, k11, t12) => {
                    if k11 != k2 {
                        Err(format!("Type argument has wrong kind"))
                    } else {
                        Ok(type_substitution(&name, t2, *t12))
                    }
                }
                _ => Err(format!("universal type expected"))
            }
        }
        // T-If
        Term::If(term1, term2, term3) => {
            let t1 = type_of(context, *term1)?;

            if !type_equivalence(context, t1, Type::Base(BaseType::Bool)) {
                return Err(format!("The type of the guard needs to be a Bool"));
            }

            let t2 = type_of(context, *term2)?;
            let t3 = type_of(context, *term3)?;

            if !type_equivalence(context, t2.clone(), t3) {
                return Err(format!("The type of the terms of the branches do not match"));
            }

            Ok(t2)
        }
        // T-While
        Term::While(term1, term2) => {
            let t1 = type_of(context, *term1)?;

            if !type_equivalence(context, t1, Type::Base(BaseType::Bool)) {
                return Err(format!("The type of the guard needs to be a Bool"));
            }

            let t2 = type_of(context, *term2)?;

            if !type_equivalence(context, t2.clone(), Type::Base(BaseType::Unit)) {
                return Err(format!("The type of the body should be unit"));
            }

            Ok(Type::Base(BaseType::Unit))
        }
        // T-True, T-False
        Term::True | Term::False => Ok(Type::Base(BaseType::Bool)),
        // T-Integer
        Term::Integer(_) => Ok(Type::Base(BaseType::Int)),
        // T-Float
        Term::Float(_) => Ok(Type::Base(BaseType::Float)),
        // T-Unit
        Term::Unit => Ok(Type::Base(BaseType::Unit)),
        // T-Reference
        Term::Reference(term) => {
            let t1 = type_of(context, *term)?;

            Ok(Type::Reference(Box::new(t1)))
        }
        // T-Let
        Term::Let(x, term1, term2) => {
            let t1 = type_of(context, *term1)?;

            let mut new_context = context.add_binding(Binding::VarBinding(x, t1));

            let t2 = type_of(&mut new_context, *term2)?;

            Ok(t2)
        }
        // T-Tuple
        Term::Tuple(terms) => {
            let mut types = Vec::new();

            for term in terms {
                let t = type_of(context, term)?;
                types.push(t);
            }

            Ok(Type::Tuple(types))
        }
        // T-TupleProj
        Term::TupleProjection(term, index) => {
            let t = type_of(context, *term)?;
            match simplify_type(context, t) {
                Type::Tuple(types) => {
                    if index >= types.len() {
                        return Err(format!("Trying to project a tuple with an index out of range"));
                    }
                    Ok(types[index].clone())
                }
                _ => Err(format!("Only Tuples can be projected with indexes"))
            }
        }
        // T-Rcd
        Term::Record(terms) => {
            let mut types = HashMap::new();

            for (label, term) in terms {
                let t = type_of(context, term)?;
                types.insert(label, t);
            }

            Ok(Type::Record(types))
        }
        // T-Proj
        Term::RecordProjection(term, label) => {
            let t = type_of(context, *term)?;

            match simplify_type(context, t) {
                Type::Record(types) => {
                    match types.get(&label) {
                        None => Err(format!("Projected with a label not in the record")),
                        Some(t) => Ok(t.clone()),
                    }
                }
                t => Err(format!("Only records can be projected with labels, found: {:?}", t))
            }
        }
        // T-Variant
        Term::Tagging(tag, term, t1) => {
            let t = type_of(context, *term)?;

            match simplify_type(context, t1.clone()) {
                // We only allow tagging with a Variant type.
                Type::Variants(variants) => {
                    // We need to check what label the term is, and make sure it is actually one of the variants.
                    match variants.get(&tag).cloned() {
                        Some(case) => {
                            // We need the type of the term to be the same as specified in the variants.
                            if type_equivalence(context, case, t) {
                                Ok(t1)
                            } else {
                                Err(format!("The type does not match the type specified by the variant with the same label"))
                            }
                        }
                        None => Err(format!("The label used is not a member of the variant"))
                    }
                }
                _ => Err(format!("A tagging need to be tagged with a variant type, or something that simplifies to a variant type"))
            }
        }
        // T-Case
        Term::Case(term, cases) => {
            let t = type_of(context, *term)?;
            match simplify_type(context, t) {
                Type::Variants(types) => {
                    if types.len() > cases.len() {
                        return Err("Not all cases are covered".to_string());
                    }
                    if types.len() < cases.len() {
                        return Err("Too many cases for variant".to_string());
                    }

                    let mut case_types = Vec::new();

                    for (label, (binding, inner_term)) in cases {
                        match types.get(&label) {
                            Some(ty) => {
                                let mut new_context = context.add_binding(Binding::VarBinding(binding, ty.clone()));

                                let t = type_of(&mut new_context, inner_term)?;
                                case_types.push(t);
                            }
                            None => return Err(format!("The case label is not specified in the type of the term, and can therefore not match"))
                        }
                    }

                    // Check that the return type of all cases are equal
                    if let Some(first) = case_types.first() {
                        let equal = case_types.iter().cloned().all(|a| type_equivalence(context, first.clone(), a));
                        if equal {
                            Ok(first.clone())
                        } else {
                            Err(format!("Not all cases in the case term return the same type as required"))
                        }
                    } else {
                        Err(format!("The case term needs at least one case to type check"))
                    }
                }
                _ => Err(format!("You can only use a case on a term that is a variant."))
            }
        }
        // T-Ascribe
        Term::Ascribe(t, ty) => {
            //check_kind_star(context, ty.clone()); // Todo: We might need to re-add this, but it will fail if we try to ascribe with a defined type, because we cant check the kind of that.

            let term_type = type_of(context, *t)?;

            if type_equivalence(context, term_type.clone(), ty.clone()) {
                Ok(ty)
            } else {
                // Todo: Consider extracting this to a try_get_type
                let t = match ty {
                    TypeVar(i) => context.get_type(&i)?,
                    l => l,
                };
                Err(format!("The ascription expected the type to be: {}, but it was: {}, in Context: {:?}", t.to_string_type(context, 0), term_type.to_string_type(context, 0), context))
            }
        }
        // T-Define
        Term::Define(x, ty, term) => {
            let mut new_context = context.add_binding(Binding::VarBinding(x.clone(), ty.clone()));
            let t = type_of(&mut new_context, *term)?;

            Ok(type_substitution(&x, ty, t))
        }
        // T-Scope
        Term::Scope(term) => {
            type_of(context, *term)
        }
        // T-Seq
        Term::Seq(term1, term2) => {
            let t1 = type_of(context, *term1)?; // Todo: If we convert to result, we need to handle that here.

            let t2 = type_of(context, *term2)?;
            Ok(t2)
        }
        // T-Assign
        Term::Assignment(x, term) => {
            let ty = context.get_type(&x)?;
            if !type_equivalence(context, type_of(context, *term)?, ty.clone()) {
                panic!("The assigned term needs to have the same type as the variable assigned to.")
            }
            Ok(ty)
        }
        Term::Replacement => Ok(Type::Base(BaseType::Unit)),
        // T-Deref
        Term::DeReference(r) => {
            let t = type_of(context, *r)?;

            match simplify_type(context, t) {
                Type::Reference(ty) => {
                    Ok(*ty)
                }
                _ => Err(format!("Can only dereference Reference types"))
            }
        }
    }
}