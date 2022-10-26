use std::collections::HashMap;
use std::ops::DerefMut;
use crate::{add_name, BaseType, Binding, Constraint, Context, get_binding, get_kind, class_exists, get_type, get_type_safe, Kind, Substitutions, Term, Type, type_substitution, ZeroOneMore, unify};
use crate::Type::{Qualified, TypeApp, TypeArrow, TypeVar};

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
                Type::TypeVar(name) => {
                    match get_type_safe(context, &name) {
                        Ok(t) => return compute_type(context, Type::TypeApp(Box::new(t), tyT2)),
                        Err(_) => return Err(Type::TypeApp(Box::new(Type::TypeVar(name)), tyT2))
                    }
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

    //println!("Test equivalence between: {} and {} with context: {:?}", simple_tyt.to_string_type(context, 0), simple_tys.to_string_type(context, 0), context);

    match (simple_tys, simple_tyt) {
        (Type::TypeArrow(s1, s2), Type::TypeArrow(t1, t2)) => {
            type_equivalence(context, *s1, *t1) && type_equivalence(context, *s2, *t2)
        }
        (Type::TypeVar(i), Type::TypeVar(j)) => {
            match (get_type_safe(context, &i), get_type_safe(context, &j)) {
                (Ok(ti), Ok(tj)) => type_equivalence(context, ti, tj),
                (Ok(ti), Err(_)) => type_equivalence(context, ti, Type::TypeVar(j)),
                (Err(_), Ok(tj)) => type_equivalence(context, Type::TypeVar(i), tj),
                (Err(_), Err(_)) => i == j, // Todo: This is only correct if all binding names are unique
            }
        },
        (ty, Type::TypeVar(i)) | (Type::TypeVar(i), ty) => {
            type_equivalence(context, get_type(context, &i), ty)
        }
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
        Type::Qualified(_, _) => {
            Kind::KindStar // Todo: What kind should we return?
        }
    }
}

pub fn check_kind_star(context: &Context, tyt: Type) {
    if kind_of(context, tyt) != Kind::KindStar {
        panic!("Kind * expected")
    }
}

// TYPING
/// Check the type of a given term with the context. We need all classes to be the first,
/// and instances to be the second.
pub fn type_of(context: &mut Context, term: Term, substitutions: &mut Substitutions) -> Type {
    match term {
        // T-Var
        Term::TermVar(name) => {
            get_type(context, &name)
            /*match context.potential_candidates(&name) {
                ZeroOneMore::Zero => panic!("Unbound variable in context: {:?}", context),
                ZeroOneMore::One(t) => t,
                ZeroOneMore::More(_) => panic!("Multiple potential candidates. Context: {:?}", context)
            }*/
        },
        // T-Abs
        Term::TermAbs(x, t1, term2) => {
            //check_kind_star(context, t1.clone()); todo: Re-add

            let mut new_context = context.add_binding(Binding::VarBinding(x, t1.clone()));

            let t2 = type_of(&mut new_context, *term2, substitutions);
            Type::TypeArrow(Box::new(t1), Box::new(t2))
        }
        // T-App
        Term::TermApp(term1, term2) => {
            println!("trying to type check {:?} {:?}", term1, term2);
            let mut t2 = type_of(context, *term2, substitutions);
            let mut t1 = type_of(context, *term1, substitutions);

            let mut result = Type::TypeArrow(Box::new(t2.clone()), Box::new(Type::TypeVar("#NEW".to_string())));

            result = unify(context, substitutions, t1, result, vec![], vec![]).unwrap().1;

            result = match result {
                Type::TypeArrow(_, x) => *x,
                _ => panic!("Must be a type application (should be a function type), found {:?}", result)
            };
            result

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

            let t2 = type_of(&mut new_context, *term2, substitutions);

            Type::TypeAll(x, k1, Box::new(t2))
        }
        // T-TApp
        Term::TermTypeApp(term1, t2) => {
            let k2 = kind_of(context, t2.clone());
            let t1 = type_of(context, *term1, substitutions);
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
            let t1 = type_of(context, *term1, substitutions);
            let t1 = simplify_type(context, t1);

            if !type_equivalence(context, t1, Type::Base(BaseType::Bool)) {
                panic!("The type of the guard needs to be a Bool");
            }

            let t2 = type_of(context, *term2, substitutions);
            let t2 = simplify_type(context, t2);

            let t3 = type_of(context, *term3, substitutions);
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
            let t1 = type_of(context, *term, substitutions);
            let t1 = simplify_type(context, t1);

            Type::Reference(Box::new(t1))
        }
        // T-Let
        Term::Let(x, term1, term2) => {
            let t1 = type_of(context, *term1, substitutions);
            let t1 = simplify_type(context, t1);

            let mut new_context = context.add_binding(Binding::VarBinding(x, t1));

            let t2 = type_of(&mut new_context, *term2, substitutions);

            t2
        }
        // T-Tuple
        Term::Tuple(terms) => {
            let mut types = Vec::new();

            for term in terms {
                let t = type_of(context, term, substitutions);
                let t = simplify_type(context, t);
                types.push(t);
            }

            Type::Tuple(types)
        }
        // T-TupleProj
        Term::TupleProjection(term, index) => {
            let t = type_of(context, *term, substitutions);
            match simplify_type(context, t) {
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
                let t = type_of(context, term, substitutions);
                let t = simplify_type(context, t);
                types.insert(label, t);
            }

            Type::Record(types)
        }
        // T-Proj
        Term::RecordProjection(term, label) => {
            let t = type_of(context, *term, substitutions);
            match simplify_type(context, t) {
                Type::Record(types) => {
                    match types.get(&label) {
                        None => panic!("Projected with a label not in the record"),
                        Some(t) => t.clone(),
                    }
                }
                t => panic!("Only records can be projected with labels, found: {:?}", t)
            }
        }
        // T-Variant
        Term::Tagging(tag, term, t1) => {
            let t = type_of(context, *term, substitutions);
            let t = simplify_type(context, t);

            match simplify_type(context, t1.clone()) {
                // We only allow tagging with a Variant type.
                Type::Variants(variants) => {
                    // We need to check what label the term is, and make sure it is actually one of the variants.
                    match variants.get(&tag).cloned() {
                        Some(case) => {
                            // We need the type of the term to be the same as specified in the variants.
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
            let t = type_of(context, *term, substitutions);
            match simplify_type(context, t) {
                Type::Variants(types) => {
                    let mut case_types = Vec::new();

                    for (label, (binding, inner_term)) in cases {
                        match types.get(&label) {
                            Some(ty) => {
                                let mut new_context = context.add_binding(Binding::VarBinding(binding, ty.clone()));

                                let t = type_of(&mut new_context, inner_term, substitutions);
                                let inner_term_type = simplify_type(&new_context, t);
                                case_types.push(inner_term_type);
                            }
                            None => panic!("The case label is not specified in the type of the term, and can therefore not match")
                        }
                    }

                    // Check that the return type of all cases are equal
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
                    Type::TypeArrow(Box::new(type_substitution(&label, ty.clone(), *ty2)), Box::new(ty))
                }
                _ => panic!("Only recursive types can be folded")
            }
        }
        // T-UnFold
        Term::UnFold(ty) => {
            match simplify_type(context, ty.clone()) {
                Type::Recursive(label, kn, ty2) => {
                    Type::TypeArrow(Box::new(ty.clone()), Box::new(type_substitution(&label,  ty, *ty2)))
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

                    let tyU = type_of(context, *t2, substitutions);
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
            let tyT1 = type_of(context, *t1, substitutions);
            match simplify_type(context, tyT1) {
                Type::Existential(tyY,k,tyT11) => {
                    // Add X
                    let context_new = context.add_binding(Binding::TyVarBinding(tyX, k));
                    // Add x
                    let mut context_new = context_new.add_binding(Binding::VarBinding(x, *tyT11));

                    type_of(&mut context_new, *t2, substitutions)
                }
                _ => panic!("You can only pack existential types")
            }
        }
        // T-Ascribe
        Term::Ascribe(t, ty) => {
            //check_kind_star(context, ty.clone()); // Todo: We might need to re-add this, but it will fail if we try to ascribe with a defined type, because we cant check the kind of that.

            let term_type = type_of(context, *t, substitutions);

            if type_equivalence(context, term_type.clone(), ty.clone()) {
                term_type //ty // <- The ty was there before. The two have two different semantics.
                // If ty then if we ascribe with a tyVar which is common, we will return a TyVar which is not fun for those
                // checking the type like TupleProjection and friends.
            } else {
                // Todo: Consider extracting this to a try_get_type
                let t = match ty {
                    Type::TypeVar(i) => get_type(context, &i),
                    l => l,
                };
                panic!("The ascription expected the type to be: {}, but it was: {}, in Context: {:?}", t.to_string_type(context, 0), term_type.to_string_type(context, 0), context)
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
            let t1 = type_of(context, *term1, substitutions); // Todo: If we convert to result, we need to handle that here.
            let _ = simplify_type(context, t1);

            let t2 = type_of(context, *term2, substitutions);
            simplify_type(context, t2)
        }
        // T-Fix
        Term::Fix(t) => {
            let t = type_of(context, *t, substitutions);
            let t1 = simplify_type(context, t);

            match t1 {
                Type::TypeArrow(t11, t12) => {
                    if type_equivalence(context, *t11, *t12.clone()) {
                        *t12
                    } else {
                        panic!("The result of the body is not compatible with the domain. Expected the types to be equal.")
                    }
                }
                _ => panic!("Only arrow types can be fixed.")
            }
        }
        // T-Class
        Term::Class { constraints, name: class_name, vars, declarations, default_implementations, continuation } => {
            // Assert that there is no existing overloaded binding in the context
            if class_exists(context, &class_name) {
                panic!("The type already has a overload with the label. Shadowing is not allowed for type-classes.")
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
            if !class_exists(context, &class_name) {
                panic!("The class name was missing in context: {:?}", context)
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
    }
}

/// Tries to distribute qualifications
fn distribute_qualification(ty: Type) -> Type {
    match ty {
        Type::Qualified(constraints, inner) => {
            match *inner {
                Type::TypeVar(s) => TypeVar(s),
                Type::TypeArrow(ty1, ty2) => {
                    Type::TypeArrow(Box::new(Qualified(constraints.clone(), ty1)), Box::new(Qualified(constraints.clone(), ty2)))
                }
                x => Qualified(constraints, Box::new(x))
            }
        }
        x => x,
    }
}

pub fn bind_variable(context: &mut Context, subs: &mut Substitutions, var: &String, typ: &Type, constraints: Vec<Constraint>) -> Result<(), String> {
    println!("Bind var {:?} to {}, with constraints {:?}", var, typ.to_string_type(context, 0), &constraints);
    //println!("{:#?}", context);
    match typ {
        TypeVar(var2) => {
            if var != var2 {
                subs.insert(var.clone(), typ.clone());
                /*subs.insert(
                    var.clone(),
                    Type::qualified(subs.apply_consts(constraints), typ.clone())
                );*/

                /*
                // In all substitutions replace the old var with the new type
                for (_, v) in &mut subs.subs {
                    replace_var(v, var, typ);
                }
                */

                /*let cons = {
                    let con = &mut *context.constraints.borrow_mut();
                    con.remove(var)
                };

                match cons {
                    Some(constraints) => {
                        for c in constraints.iter() {
                            context.insert_constraint(var2, c);
                        }
                    }
                    None => ()
                }*/
            }
            Ok(())
        }
        _ => {
            for (_, replaced) in &subs.subs {
                println!("replace_var {} with {} in {}", replaced, var, typ.to_string_type(context, 0));
                //replace_var(replaced, var, typ);
            }

            subs.insert(var.clone(), typ.clone());

            let mut new_constraints = Vec::new();


            for c in constraints.iter() {
                // Only check constraints that are related directly to the variable currently being bound
                if c.vars.contains(&TypeVar(var.clone())) {
                    context.has_instance(&c.ident, typ, &mut new_constraints)?;
                }
            }
            for constraint in new_constraints {
                // Todo: Add transitive constraints.
                //context.insert_constraint(&constraint.variables[0], constraint.class)
            }
            Ok(())
        }
    }
}



