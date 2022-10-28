use std::collections::HashMap;
use crate::constraint::Constraint;
use crate::{Context, Term, unify};
use crate::base_type::BaseType;
use crate::binding::Binding;
use crate::substitutions::Substitutions;
use crate::type_util::type_substitution;
use crate::types::Type;
use crate::types::Type::{TypeArrow, TypeVar};

/// Check the type of a given term with the context. We need all classes to be the first,
/// and instances to be the second.
pub fn type_of(context: &Context, term: Term, substitutions: &mut Substitutions) -> Result<Type, String> {
    match term {
        // T-Var
        Term::TermVar(name) => {
            context.get_type(&name)
        },
        // T-Abs
        Term::TermAbs(x, t1, term2) => {
            //check_kind_star(context, t1.clone()); todo: Re-add

            let mut new_context = context.add_binding(Binding::VarBinding(x, t1.clone()));

            let t2 = type_of(&mut new_context, *term2, substitutions)?;
            Ok(Type::TypeArrow(Box::new(t1), Box::new(t2)))
        }
        // T-App
        Term::TermApp(term1, term2) => {
            println!("trying to type check {:?} {:?}", term1, term2);
            let mut t2 = type_of(context, *term2, substitutions)?;
            let mut t1 = type_of(context, *term1, substitutions)?;

            let mut result = TypeArrow(Box::new(t2.clone()), Box::new(TypeVar("#NEW".to_string())));

            result = unify(context, substitutions, t1, result, vec![], vec![])?;

            match result {
                TypeArrow(_, x) => Ok(*x),
                _ => Err(format!("Must be a type application (should be a function type), found {:?}", result))
            }

            /*match simplify_type(context, t1) {
                Type::TypeArrow(t11, t12) => {
                    if type_equivalence(context, t2.clone(), *t11.clone()) {
                        *t12
                    } else {
                        panic!("Cannot apply: {}, to function of type: {}", t2.to_string_type(context, 0),
                               Type::TypeArrow(t11, t12).to_string_type(context, 0)
                        )
                    }
                }
                _ => panic!("arrow type expected")
            }*/
        }
        // T-TAbs
        Term::TermTypeAbs(x, k1, term2) => {
            let mut new_context = context.add_binding(Binding::TyVarBinding(x.clone(), k1.clone()));

            let t2 = type_of(&mut new_context, *term2, substitutions)?;

            Ok(Type::TypeAll(x, k1, Box::new(t2)))
        }
        // T-TApp
        Term::TermTypeApp(term1, t2) => {
            let k2 = crate::kind_of(context, t2.clone());
            let t1 = type_of(context, *term1, substitutions)?;
            match crate::type_util::simplify_type(context, t1) {
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
            let t1 = type_of(context, *term1, substitutions)?;
            let t1 = crate::type_util::simplify_type(context, t1);

            if !crate::type_util::type_equivalence(context, t1, Type::Base(BaseType::Bool)) {
                return Err(format!("The type of the guard needs to be a Bool"));
            }

            let t2 = type_of(context, *term2, substitutions)?;
            let t2 = crate::type_util::simplify_type(context, t2);

            let t3 = type_of(context, *term3, substitutions)?;
            let t3 = crate::type_util::simplify_type(context, t3);

            if !crate::type_util::type_equivalence(context, t2.clone(), t3) {
                return Err(format!("The type of the terms of the branches do not match"));
            }

            Ok(t2)
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
            let t1 = type_of(context, *term, substitutions)?;
            let t1 = crate::type_util::simplify_type(context, t1);

            Ok(Type::Reference(Box::new(t1)))
        }
        // T-Let
        Term::Let(x, term1, term2) => {
            let t1 = type_of(context, *term1, substitutions)?;
            let t1 = crate::type_util::simplify_type(context, t1);

            let mut new_context = context.add_binding(Binding::VarBinding(x, t1));

            let t2 = type_of(&mut new_context, *term2, substitutions)?;

            Ok(t2)
        }
        // T-Tuple
        Term::Tuple(terms) => {
            let mut types = Vec::new();

            for term in terms {
                let t = type_of(context, term, substitutions)?;
                let t = crate::type_util::simplify_type(context, t);
                types.push(t);
            }

            Ok(Type::Tuple(types))
        }
        // T-TupleProj
        Term::TupleProjection(term, index) => {
            let t = type_of(context, *term, substitutions)?;
            match crate::type_util::simplify_type(context, t) {
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
                let t = type_of(context, term, substitutions)?;
                let t = crate::type_util::simplify_type(context, t);
                types.insert(label, t);
            }

            Ok(Type::Record(types))
        }
        // T-Proj
        Term::RecordProjection(term, label) => {
            let t = type_of(context, *term, substitutions)?;
            match crate::type_util::simplify_type(context, t) {
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
            let t = type_of(context, *term, substitutions)?;
            let t = crate::type_util::simplify_type(context, t);

            match crate::type_util::simplify_type(context, t1.clone()) {
                // We only allow tagging with a Variant type.
                Type::Variants(variants) => {
                    // We need to check what label the term is, and make sure it is actually one of the variants.
                    match variants.get(&tag).cloned() {
                        Some(case) => {
                            // We need the type of the term to be the same as specified in the variants.
                            if crate::type_util::type_equivalence(context, case, t) {
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
            let t = type_of(context, *term, substitutions)?;
            match crate::type_util::simplify_type(context, t) {
                Type::Variants(types) => {
                    let mut case_types = Vec::new();

                    for (label, (binding, inner_term)) in cases {
                        match types.get(&label) {
                            Some(ty) => {
                                let mut new_context = context.add_binding(Binding::VarBinding(binding, ty.clone()));

                                let t = type_of(&mut new_context, inner_term, substitutions)?;
                                let inner_term_type = crate::type_util::simplify_type(&new_context, t);
                                case_types.push(inner_term_type);
                            }
                            None => return Err(format!("The case label is not specified in the type of the term, and can therefore not match"))
                        }
                    }

                    // Check that the return type of all cases are equal
                    if let Some(first) = case_types.first() {
                        let equal = case_types.iter().cloned().all(|a| crate::type_util::type_equivalence(context, first.clone(), a));
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
        // T-Fold
        Term::Fold(ty) => {
            match crate::type_util::simplify_type(context, ty.clone()) {
                Type::Recursive(label, kn, ty2) => {
                    Ok(Type::TypeArrow(Box::new(type_substitution(&label, ty.clone(), *ty2)), Box::new(ty)))
                }
                _ => Err(format!("Only recursive types can be folded"))
            }
        }
        // T-UnFold
        Term::UnFold(ty) => {
            match crate::type_util::simplify_type(context, ty.clone()) {
                Type::Recursive(label, kn, ty2) => {
                    Ok(Type::TypeArrow(Box::new(ty.clone()), Box::new(type_substitution(&label,  ty, *ty2))))
                }
                _ => Err(format!("Only recursive types can be folded"))
            }
        }
        // T-Pack
        Term::Pack(tyT1,t2,tyT) => {
            crate::check_kind_star(context, tyT.clone());

            match crate::type_util::simplify_type(context, tyT.clone()) {
                Type::Existential(tyY,k,tyT2) => {
                    if crate::kind_of(context, tyT1.clone()) != k {
                        return Err(format!("The type component does not have the correct kind"))
                    }

                    let tyU = type_of(context, *t2, substitutions)?;
                    let tyU_new = type_substitution(&tyY, tyT1, *tyT2);

                    if !crate::type_util::type_equivalence(context, tyU, tyU_new) {
                        return Err(format!("The expected existential type does not match the type of the term"))
                    }

                    Ok(tyT)
                }
                _ => Err(format!("You can only pack existential types"))
            }

        }
        // T-UnPack
        Term::UnPack(tyX,x,t1,t2) => {
            let tyT1 = type_of(context, *t1, substitutions)?;
            match crate::type_util::simplify_type(context, tyT1) {
                Type::Existential(tyY,k,tyT11) => {
                    // Add X
                    let context_new = context.add_binding(Binding::TyVarBinding(tyX, k));
                    // Add x
                    let mut context_new = context_new.add_binding(Binding::VarBinding(x, *tyT11));

                    type_of(&mut context_new, *t2, substitutions)
                }
                _ => Err(format!("You can only pack existential types"))
            }
        }
        // T-Ascribe
        Term::Ascribe(t, ty) => {
            //check_kind_star(context, ty.clone()); // Todo: We might need to re-add this, but it will fail if we try to ascribe with a defined type, because we cant check the kind of that.

            let term_type = type_of(context, *t, substitutions)?;

            if crate::type_util::type_equivalence(context, term_type.clone(), ty.clone()) {
                Ok(term_type) //ty // <- The ty was there before. The two have two different semantics.
                // If ty then if we ascribe with a tyVar which is common, we will return a TyVar which is not fun for those
                // checking the type like TupleProjection and friends.
            } else {
                // Todo: Consider extracting this to a try_get_type
                let t = match ty {
                    Type::TypeVar(i) => context.get_type(&i)?,
                    l => l,
                };
                Err(format!("The ascription expected the type to be: {}, but it was: {}, in Context: {:?}", t.to_string_type(context, 0), term_type.to_string_type(context, 0), context))
            }
        }
        // T-Define
        Term::Define(x, ty, term) => {
            let mut new_context = context.add_binding(Binding::VarBinding(x, ty));

            type_of(&mut new_context, *term, substitutions)
        }
        // T-Scope
        Term::Scope(term) => {
            type_of(context, *term, substitutions)
        }
        // T-Seq
        Term::Seq(term1, term2) => {
            let t1 = type_of(context, *term1, substitutions)?; // Todo: If we convert to result, we need to handle that here.
            let _ = crate::type_util::simplify_type(context, t1);

            let t2 = type_of(context, *term2, substitutions)?;
            Ok(crate::type_util::simplify_type(context, t2))
        }
        // T-Fix
        Term::Fix(t) => {
            let t = type_of(context, *t, substitutions)?;
            let t1 = crate::type_util::simplify_type(context, t);

            match t1 {
                Type::TypeArrow(t11, t12) => {
                    if crate::type_util::type_equivalence(context, *t11, *t12.clone()) {
                        Ok(*t12)
                    } else {
                        return Err(format!("The result of the body is not compatible with the domain. Expected the types to be equal."))
                    }
                }
                _ => return Err(format!("Only arrow types can be fixed."))
            }
        }
        // T-Class
        Term::Class { constraints, name: class_name, vars, declarations, default_implementations, continuation } => {
            // Assert that there is no existing overloaded binding in the context
            if context.class_exists(&class_name) {
                return Err(format!("The type already has a overload with the label. Shadowing is not allowed for type-classes."));
            }

            let mut new_context = context.add_binding(Binding::ClassBinding {
                constraints,
                name: class_name.clone(),
                vars: vars.clone(),
                declarations: declarations.clone(),
                default_implementations
            });

            for (name, declaration) in declarations {
                new_context = new_context.add_binding(Binding::VarBinding(name.clone(), Type::Qualified(
                    vec![Constraint { ident: class_name.clone(), vars: vars.clone() }],
                    Box::new(declaration)
                )))
            }

            type_of(&mut new_context, *continuation, substitutions)
        }
        // T-Instance
        Term::Instance{ constraints, class_name, ty, mut implementations, continuation } => {
            if !context.class_exists(&class_name) {
                return Err(format!("The class name was missing in context: {:?}", context));
            }

            // Todo: What about kinds?
            let (class_constraints, class_vars, class_declarations) = context.class_items(&class_name);

            for (name, (implementation_term, implementation_type)) in &mut implementations {
                // Check that the class declarations and implementations match and that all required methods are implemented
                match class_declarations.get(name) {
                    None => panic!("Could not find {:?} in class {:?}", name, &class_name),
                    Some(class_declaration) => {

                        // Todo: We need to somehow check that the implementation is a specialization of the class declarations type

                        /*
                            binding.typ = decl.typ.clone();

                            replace_var(&mut binding.typ.value, class_var, &instance.typ);

                            // Todo: What is freshen_qualified_type?
                            self.freshen_qualified_type(&mut binding.typ, HashMap::new());
                        */

                        /*if !type_specialization(in_context, ty, context) {
                            panic!("The type you are trying to overload is not the same as the type in the context.");
                        }*/
                    }
                }



                // Add the instances constraints to all the implementations constraints.
                *implementation_type = Type::Qualified(constraints.clone(), Box::new(implementation_type.clone()));

                // Check that all the class constraints are valid for the type of the implementation
                // This means we can not create an instance of a type-class for a type that does not
                // uphold the constraints of the class declaration
                for class_constraint in &class_constraints {
                    context.has_instance(&class_constraint.ident, &ty, &mut Vec::new()).unwrap()
                }
            }

            // Todo: Check for colliding implementations of the same Overload

            //check_overload_exists(context, x.clone(), ty.clone());

            let mut new_context = context.add_binding(Binding::InstanceBinding{
                constraints,
                class_name,
                ty,
                implementations
            });


            type_of(&mut new_context, *continuation, substitutions)
        }
        // T-Assign
        Term::Assignment(x, term) => {
            let ty = context.get_type(&x)?;
            if !crate::type_util::type_equivalence(context, type_of(context, *term, substitutions)?, ty.clone()) {
                panic!("The assigned term needs to have the same type as the variable assigned to.")
            }
            Ok(ty)
        }
    }
}