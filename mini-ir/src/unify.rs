use std::collections::HashMap;

use crate::constraint::Constraint;
use crate::Context;
use crate::substitutions::Substitutions;
use crate::type_util::bind_variable;
use crate::types::Type;
use crate::types::Type::{Qualified, TypeApp, TypeArrow, TypeVar};

pub fn unify(context: &Context, subs: &mut Substitutions, left: Type, right: Type, left_constraints: Vec<Constraint>, right_constraints: Vec<Constraint>) -> Result<Type, String> {
    println!("Try-Unify: {} ⊔ {}", left.to_string_type(context, 0), right.to_string_type(context, 0));

    match (left, right) {
        (Qualified(c1, t1), t2) => {
            let mut new_left = left_constraints.clone();
            new_left.extend(c1.clone());

            let t1 = unify(context, subs, *t1, t2, new_left, right_constraints)?;

            let t1 = Type::qualified(subs.apply_consts(c1.clone()), t1);

            Ok(t1.clone())
        }
        (t1, Qualified(c2, t2)) => {
            let mut new_right = left_constraints.clone();
            new_right.extend(c2.clone());


            let t1 = unify(context, subs, t1, *t2, left_constraints, new_right)?;

            let t1 = Type::qualified(subs.apply_consts(c2.clone()), t1);

            Ok(t1)
        }
        (Type::Existential(..), Type::Existential(..)) => {
            todo!("How do we unify two existential types")
        }
        (Type::Recursive(..), Type::Recursive(..)) => {
            todo!("How do we unify two recursive types")
        }
        (Type::TypeAll(..), Type::TypeAll(..)) => {
            todo!("How do we unify two forall types")
        }
        (Type::Reference(t1), Type::Reference(t2)) => {
            unify(context, subs, *t1, *t2, left_constraints, right_constraints)
                .map(|t1| {
                    Type::Reference(Box::new(t1))
                })
        }
        (Type::Tuple(t1), Type::Tuple(t2)) => {
            if t1.len() != t2.len() {
                return Err("The tuples are not equal length".to_string());
            }

            let mut res1 = vec![];

            for (g1, g2) in t1.into_iter().zip(t2.into_iter()) {
                let g1 = subs.apply(g1);
                let g2 = subs.apply(g2);

                match unify(context, subs, g1, g2, left_constraints.clone(), right_constraints.clone()) {
                    Ok(f1) => {
                        res1.push(f1);
                    }
                    e => return e,
                }
            }

            let tup1 = subs.apply(Type::Tuple(res1));

            Ok(tup1)
        }
        (Type::Record(h1), Type::Record(h2)) => {
            if h1.len() != h2.len() {
                return Err("The records are not equal length".to_string());
            }

            let mut h1_new = HashMap::new();

            for (k1, v1) in &h1 {
                if let Some(v2) = h2.get(k1) {
                    let v1 = subs.apply(v1.clone());
                    let v2 = subs.apply(v2.clone());

                    match unify(context, subs, v1, v2, left_constraints.clone(), right_constraints.clone()) {
                        Ok(l1) => {
                            h1_new.insert(k1.clone(), l1);
                        }
                        e => return e
                    }
                } else {
                    return Err("The fields labels does not match and are therefore not unifiable.".to_string());
                }
            }

            let rec1 = subs.apply(Type::Record(h1_new));
            Ok(rec1)
        }
        (Type::Variants(h1), Type::Variants(h2)) => {
            if h1.len() != h2.len() {
                return Err("The records are not equal length".to_string());
            }

            let mut h1_new = HashMap::new();

            for (k1, v1) in &h1 {
                if let Some(v2) = h2.get(k1) {
                    let v1 = subs.apply(v1.clone());
                    let v2 = subs.apply(v2.clone());
                    match unify(context, subs, v1, v2, left_constraints.clone(), right_constraints.clone()) {
                        Ok(l1) => {
                            h1_new.insert(k1.clone(), l1);
                        }
                        e => return e
                    }
                } else {
                    return Err("The fields label does not match and are therefore not unifiable.".to_string());
                }
            }

            let rec1 = subs.apply(Type::Variants(h1_new));

            Ok(rec1)
        }
        (TypeArrow(l1, r1), TypeArrow(l2, r2)) => {
            unify(context, subs, *l1, *l2, left_constraints.clone(), right_constraints.clone())
                .and_then(|l1| {
                    let r1 = subs.apply(*r1);
                    let r2 = subs.apply(*r2);
                    unify(context, subs, r1, r2, left_constraints, right_constraints)
                        .map(|r1| {
                            TypeArrow(Box::new(l1), Box::new(r1))
                        })
                })
        }
        (TypeApp(l1, r1), TypeApp(l2, r2)) => {
            unify(context, subs, *l1, *l2, left_constraints.clone(), right_constraints.clone())
                .and_then(|l1| {
                    let r1 = subs.apply(*r1);
                    let r2 = subs.apply(*r2);
                    unify(context, subs, r1, r2, left_constraints, right_constraints)
                        .map(|r1| {
                            TypeApp(Box::new(l1), Box::new(r1))
                        })
                })
        }
        (Type::Base(b1), Type::Base(b2)) => {
            if b1 == b2 {
                Ok(Type::Base(b1))
            } else {
                Err(format!("{:?} and {:?} can not be unified", b1, b2))
            }
        }
        (Type::TypeAbs(x1, _, t1), Type::TypeAbs(x2, _, t2)) => {
            // Substitute all x2 with x1 in the second part

            // Check the kind is the same for the two

            // Check that the t1 and t2 are unifiable

            // Remove substitutions from the map afterwards. This might not be needed if we have unique names.

            if x1 == x2 {
                Ok(todo!())
            } else {
                Err("The abs are not the same name".to_string())
            }
        }
        (TypeVar(x1), TypeVar(x2)) => {
            // Todo: Consider variable age like haskell-compiler
            let res = bind_variable(context, subs, &x1, &TypeVar(x2.clone()), left_constraints);
            match res {
                Ok(_) => {
                    let res = subs.apply(TypeVar(x1));
                    Ok(res)
                }
                Err(e) => Err(e)
            }
        }
        (TypeVar(lab1), t2) => {
            let res = bind_variable(context, subs, &lab1, &t2, left_constraints);
            match res {
                Ok(_) => {
                    Ok(t2)
                }
                Err(e) => {
                    Err(e)
                }
            }
        }
        (t1, TypeVar(lab2)) => {
            let res = bind_variable(context, subs, &lab2, &t1, right_constraints);
            match res {
                Ok(_) => {
                    Ok(t1)
                }
                Err(e) => {
                    Err(e)
                }
            }
        }
        (t1, t2) => {
            Err(format!("The type: {} could not be unified with {}", t1.to_string_type(context, 0), t2.to_string_type(context, 0)))
        }
    }
}

mod tests {
    mod base {
        use crate::{Context, unify};
        use crate::base_type::BaseType::{Float, Int};
        use crate::substitutions::Substitutions;
        use crate::types::Type;

        #[test]
        fn success() {
            // Arrange
            let t1 = Type::Base(Int);
            let t2 = Type::Base(Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn fail() {
            // Arrange
            let t1 = Type::Base(Int);
            let t2 = Type::Base(Float);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }
    }

    mod var {
        use crate::{Context, unify};
        use crate::base_type::BaseType;
        use crate::base_type::BaseType::Int;
        use crate::substitutions::Substitutions;
        use crate::types::Type;

        #[test]
        fn simple_type() {
            // Arrange
            let t1 = Type::TypeVar("x".to_string());
            let t2 = Type::Base(BaseType::Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn simple_type_reverse() {
            // Arrange
            let t1 = Type::Base(BaseType::Int);
            let t2 = Type::TypeVar("x".to_string());

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn arrow_type() {
            // Arrange
            let t1 = Type::TypeVar("x".to_string());
            let t2 = Type::arrow("y", Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn multiple() {
            // Arrange
            let t1 = Type::TypeVar("x".to_string());
            let t2 = Type::TypeVar("y".to_string());

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }
    }

    mod arrow {
        use crate::{Context, unify};
        use crate::base_type::BaseType::{Bool, Float, Int};
        use crate::substitutions::Substitutions;
        use crate::types::Type;

        #[test]
        fn simple_arrow() {
            // Arrange
            let t1 = Type::arrow("x", Int);
            let t2 = Type::arrow(Int, Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn simple_arrow_fail() {
            // Arrange
            let t1 = Type::arrow(Bool, Int);
            let t2 = Type::arrow(Int, Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn multiple_vars() {
            // Arrange
            let t1 = Type::arrow("x", Int);
            let t2 = Type::arrow(Int, "y");

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn multiple_vars_fail() {
            // Arrange
            let t1 = Type::arrow("x", Float);
            let t2 = Type::arrow(Int, "x");

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn plain_fail_left() {
            // Arrange
            let t1 = Type::arrow(Bool, Float);
            let t2 = Type::arrow(Int, "x");

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn plain_fail_right() {
            // Arrange
            let t1 = Type::arrow(Int, Float);
            let t2 = Type::arrow(Int, Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn nested_success() {
            // Arrange
            let t1 = Type::arrow(Int, Type::arrow("x", Bool));
            let t2 = Type::arrow("x", Type::arrow(Int, Bool));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn nested_different_sides() {
            // Arrange
            let t1 = Type::arrow(Int, Type::arrow("x", Bool));
            let t2 = Type::arrow(Type::arrow(Int, Bool), Bool);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }
    }

    mod reference {
        use crate::{Context, unify};
        use crate::base_type::BaseType::{Bool, Int};
        use crate::substitutions::Substitutions;
        use crate::types::Type;

        #[test]
        fn success() {
            // Arrange
            let t1 = Type::reference("x");
            let t2 = Type::reference(Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn fail() {
            // Arrange
            let t1 = Type::reference(Bool);
            let t2 = Type::reference(Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }
    }

    mod forall {
        use crate::{Context, unify};
        use crate::base_type::BaseType;
        use crate::kind::Kind::KindStar;
        use crate::substitutions::Substitutions;
        use crate::types::Type;

        #[test]
        fn same_success() {
            // Arrange
            let t1 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::Base(BaseType::Int)));
            let t2 = Type::TypeAll("X".to_string(), KindStar, Box::new(Type::Base(BaseType::Int)));

            println!("Unify: {} ⊔ {}", t1.to_string_type(&Context::new(), 0), t2.to_string_type(&Context::new(), 0));

            // Act
            let unification = unify(&mut Context::new(), &mut Substitutions::new(), t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }
    }

    mod tuple {
        use crate::{Context, unify};
        use crate::base_type::BaseType::{Bool, Int};
        use crate::substitutions::Substitutions;
        use crate::types::Type;

        #[test]
        fn pair_success() {
            // Arrange
            let t1 = Type::pair("x", Int);
            let t2 = Type::pair(Int, Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn pair_error() {
            // Arrange
            let t1 = Type::pair("x", Int);
            let t2 = Type::pair(Int, Bool);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn pair_nested() {
            // Arrange
            let t1 = Type::pair("x", Type::pair(Int, Int));
            let t2 = Type::pair(Int, Type::pair(Int, Int));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn pair_nested_fail() {
            // Arrange
            let t1 = Type::pair("x", Type::pair(Int, Int));
            let t2 = Type::pair(Type::pair(Int, Int), Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn pair_nested_vars() {
            // Arrange
            let t1 = Type::pair("x", Type::pair(Int, Int));
            let t2 = Type::pair(Type::pair(Int, Int), "y");

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn pair_same_var_multiple_subs() {
            // Arrange
            let t1 = Type::pair("x", "x");
            let t2 = Type::pair(Int, "y");

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert_eq!(unification, Ok(Type::pair(Int, Int)))
        }

        #[test]
        fn pair_same_var_multiple_subs_reversed() {
            // Arrange
            let t1 = Type::pair("x", "x");
            let t2 = Type::pair("y", Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert_eq!(unification, Ok(Type::pair(Int, Int)))
        }

        #[test]
        fn pair_same_var_multiple_subs_fail() {
            // Arrange
            let t1 = Type::pair("x", "x");
            let t2 = Type::pair(Int, Bool);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }
    }

    mod record {
        use std::collections::HashMap;

        use crate::{Context, unify};
        use crate::base_type::BaseType::{Bool, Float, Int};
        use crate::substitutions::Substitutions;
        use crate::types::Type;

        #[test]
        fn zero_element_records() {
            // Arrange
            let t1 = Type::Record(HashMap::new());
            let t2 = Type::Record(HashMap::new());

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn one_element_same_label_and_type() {
            // Arrange
            let t1 = Type::Record(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Record(HashMap::from([("x".to_string(), Type::Base(Int))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn one_element_same_label_different_type() {
            // Arrange
            let t1 = Type::Record(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Record(HashMap::from([("x".to_string(), Type::Base(Bool))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn one_element_different_label_different_type() {
            // Arrange
            let t1 = Type::Record(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Record(HashMap::from([("y".to_string(), Type::Base(Bool))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn one_element_different_label_same_type() {
            // Arrange
            let t1 = Type::Record(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Record(HashMap::from([("y".to_string(), Type::Base(Int))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn different_lengths() {
            // Arrange
            let t1 = Type::Record(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Record(HashMap::new());

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn one_element_with_var1() {
            // Arrange
            let t1 = Type::Record(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Record(HashMap::from([("x".to_string(), Type::TypeVar("X".to_string()))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn one_element_with_var2() {
            // Arrange
            let t1 = Type::Record(HashMap::from([("x".to_string(), Type::TypeVar("X".to_string()))]));
            let t2 = Type::Record(HashMap::from([("x".to_string(), Type::Base(Int))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn same_var_different_types() {
            // Arrange
            let t1 = Type::Record(HashMap::from([
                ("x".to_string(), Type::TypeVar("X".to_string())),
                ("y".to_string(), Type::TypeVar("X".to_string())),
            ]));
            let t2 = Type::Record(HashMap::from([
                ("x".to_string(), Type::Base(Int)),
                ("y".to_string(), Type::Base(Float)),
            ]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn same_var_same_types() {
            // Arrange
            let t1 = Type::Record(HashMap::from([
                ("x".to_string(), Type::TypeVar("X".to_string())),
                ("y".to_string(), Type::TypeVar("X".to_string())),
                ("z".to_string(), Type::TypeVar("X".to_string())),
            ]));
            let t2 = Type::Record(HashMap::from([
                ("x".to_string(), Type::Base(Int)),
                ("y".to_string(), Type::Base(Int)),
                ("z".to_string(), Type::Base(Int)),
            ]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn same_var_same_types_or_var() {
            // Arrange
            let t1 = Type::Record(HashMap::from([
                ("x".to_string(), Type::TypeVar("X".to_string())),
                ("y".to_string(), Type::TypeVar("X".to_string())),
                ("z".to_string(), Type::TypeVar("X".to_string())),
            ]));
            let t2 = Type::Record(HashMap::from([
                ("x".to_string(), Type::Base(Int)),
                ("y".to_string(), Type::TypeVar("Y".to_string())),
                ("z".to_string(), Type::TypeVar("Y".to_string())),
            ]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert_eq!(unification, Ok(
                Type::Record(HashMap::from([
                    ("x".to_string(), Type::Base(Int)),
                    ("y".to_string(), Type::Base(Int)),
                    ("z".to_string(), Type::Base(Int)),
                ]))
            ))
        }
    }

    mod variants {
        use std::collections::HashMap;

        use crate::{Context, unify};
        use crate::base_type::BaseType::{Bool, Float, Int};
        use crate::substitutions::Substitutions;
        use crate::types::Type;

        #[test]
        fn zero_element_variants() {
            // Arrange
            let t1 = Type::Variants(HashMap::new());
            let t2 = Type::Variants(HashMap::new());

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn one_element_same_label_and_type() {
            // Arrange
            let t1 = Type::Variants(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Variants(HashMap::from([("x".to_string(), Type::Base(Int))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn one_element_same_label_different_type() {
            // Arrange
            let t1 = Type::Variants(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Variants(HashMap::from([("x".to_string(), Type::Base(Bool))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn one_element_different_label_different_type() {
            // Arrange
            let t1 = Type::Variants(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Variants(HashMap::from([("y".to_string(), Type::Base(Bool))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn one_element_different_label_same_type() {
            // Arrange
            let t1 = Type::Variants(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Variants(HashMap::from([("y".to_string(), Type::Base(Int))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn different_lengths() {
            // Arrange
            let t1 = Type::Variants(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Variants(HashMap::new());

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn one_element_with_var1() {
            // Arrange
            let t1 = Type::Variants(HashMap::from([("x".to_string(), Type::Base(Int))]));
            let t2 = Type::Variants(HashMap::from([("x".to_string(), Type::TypeVar("X".to_string()))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn one_element_with_var2() {
            // Arrange
            let t1 = Type::Variants(HashMap::from([("x".to_string(), Type::TypeVar("X".to_string()))]));
            let t2 = Type::Variants(HashMap::from([("x".to_string(), Type::Base(Int))]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn same_var_different_types() {
            // Arrange
            let t1 = Type::Variants(HashMap::from([
                ("x".to_string(), Type::TypeVar("X".to_string())),
                ("y".to_string(), Type::TypeVar("X".to_string())),
            ]));
            let t2 = Type::Variants(HashMap::from([
                ("x".to_string(), Type::Base(Int)),
                ("y".to_string(), Type::Base(Float)),
            ]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)))
        }

        #[test]
        fn same_var_same_types() {
            // Arrange
            let t1 = Type::Variants(HashMap::from([
                ("x".to_string(), Type::TypeVar("X".to_string())),
                ("y".to_string(), Type::TypeVar("X".to_string())),
                ("z".to_string(), Type::TypeVar("X".to_string())),
            ]));
            let t2 = Type::Variants(HashMap::from([
                ("x".to_string(), Type::Base(Int)),
                ("y".to_string(), Type::Base(Int)),
                ("z".to_string(), Type::Base(Int)),
            ]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)))
        }

        #[test]
        fn same_var_same_types_or_var() {
            // Arrange
            let t1 = Type::Variants(HashMap::from([
                ("x".to_string(), Type::TypeVar("X".to_string())),
                ("y".to_string(), Type::TypeVar("X".to_string())),
                ("z".to_string(), Type::TypeVar("X".to_string())),
            ]));
            let t2 = Type::Variants(HashMap::from([
                ("x".to_string(), Type::Base(Int)),
                ("y".to_string(), Type::TypeVar("Y".to_string())),
                ("z".to_string(), Type::TypeVar("Y".to_string())),
            ]));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new();

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert_eq!(unification, Ok(
                Type::Variants(HashMap::from([
                    ("x".to_string(), Type::Base(Int)),
                    ("y".to_string(), Type::Base(Int)),
                    ("z".to_string(), Type::Base(Int)),
                ]))
            ))
        }
    }

    mod quantify {
        use std::collections::HashMap;

        use crate::{Context, Term, unify};
        use crate::base_type::BaseType::{Bool, Int};
        use crate::binding::Binding;
        use crate::constraint::Constraint;
        use crate::kind::Kind::KindStar;
        use crate::substitutions::Substitutions;
        use crate::types::Type;
        use crate::types::Type::{Base, TypeApp, TypeVar};

        fn clone_bindings() -> Vec<Binding> {
            vec![
                Binding::ClassBinding {
                    constraints: vec![],
                    name: "Clone".to_string(),
                    vars: vec![TypeVar("T".to_string())],
                    declarations: HashMap::from([
                        ("clone".to_string(), Type::arrow(Type::reference("T"), "T"))
                    ]),
                    default_implementations: Default::default(),
                },
                Binding::VarBinding(
                    "clone".to_string(),
                    Type::qualified(
                        vec![Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("T".to_string())] }],
                        Type::arrow(Type::reference("T"), "T"),
                    ),
                ),
            ]
        }

        fn inst_clone_binding(ty: Type) -> Vec<Binding> {
            vec![
                Binding::InstanceBinding {
                    constraints: vec![],
                    class_name: "Clone".to_string(),
                    ty,
                    implementations: HashMap::from([
                        ("clone".to_string(), (Term::Unit, Type::arrow(Type::reference("T"), "T")))
                    ]),
                }
            ]
        }

        fn vec_bindings() -> Vec<Binding> {
            let vec_type = Type::TypeAbs("X".to_string(), KindStar, Box::new(Type::TypeVar("X".to_string())));
            vec![
                Binding::InstanceBinding {
                    constraints: vec![
                        Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("Y".to_string())] }
                    ],
                    class_name: "Clone".to_string(),
                    ty: TypeApp(Box::new(TypeVar("Vec".to_string())), Box::new(TypeVar("Y".to_string()))),
                    implementations: HashMap::from([
                        ("clone".to_string(), (Term::Unit, Type::arrow(Type::reference("T"), "T")))
                    ]),
                },
                Binding::VarBinding("Vec".to_string(), vec_type),
            ]
        }

        #[test]
        fn base_and_qualified_base() {
            // Arrange
            let t1 = Type::qualified(
                vec![Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("T".to_string())] }],
                Base(Int),
            );
            let t2 = Base(Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new()
                .add_bindings(clone_bindings())
                .add_bindings(inst_clone_binding(Type::Base(Bool)));

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)));
            println!("Result pretty: {}", unification.map(|a| a.to_string_type(&context, 0)).unwrap_or("Error".to_string()));
        }

        #[test]
        fn base_and_qualified_base_different() {
            // Arrange
            let t1 = Type::qualified(
                vec![Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("T".to_string())] }],
                Base(Int),
            );
            let t2 = Type::Base(Bool);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new()
                .add_bindings(clone_bindings())
                .add_bindings(inst_clone_binding(Type::Base(Bool)));

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)));
            println!("Result pretty: {}", unification.map(|a| a.to_string_type(&context, 0)).unwrap_or("Error".to_string()));
        }

        #[test]
        fn clone_t_and_int_with_context_containing_int() {
            // Arrange
            let t1 = Type::qualified(
                vec![Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("T".to_string())] }],
                TypeVar("T".to_string()),
            );
            let t2 = Base(Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new()
                .add_bindings(clone_bindings())
                .add_bindings(inst_clone_binding(Base(Int)));

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)));
            println!("Result pretty: {}", unification.map(|a| a.to_string_type(&context, 0)).unwrap_or("Error".to_string()));
        }

        #[test]
        fn clone_t_and_int_with_context_not_containing_int() {
            // Arrange
            let t1 = Type::qualified(
                vec![Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("T".to_string())] }],
                Type::TypeVar("T".to_string()),
            );
            let t2 = Type::Base(Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new()
                .add_bindings(clone_bindings())
                .add_bindings(inst_clone_binding(Type::Base(Bool)));

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)));
            println!("Result pretty: {}", unification.map(|a| a.to_string_type(&context, 0)).unwrap_or("Error".to_string()));
        }

        #[test]
        fn unrelated_constraint() {
            // Arrange
            let t1 = Type::qualified(
                vec![Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("T".to_string())] }],
                Type::TypeVar("U".to_string()),
            );
            let t2 = Type::Base(Int);

            let mut substitutions = Substitutions::new();
            let mut context = Context::new()
                .add_bindings(clone_bindings())
                .add_bindings(inst_clone_binding(Type::Base(Bool)));

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)));
            println!("Result pretty: {}", unification.map(|a| a.to_string_type(&context, 0)).unwrap_or("Error".to_string()));
        }

        #[test]
        fn different_vars() {
            // Arrange
            let t1 = Type::qualified(
                vec![Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("T".to_string())] }],
                TypeVar("T".to_string()),
            );
            let t2 = TypeVar("U".to_string());

            let mut substitutions = Substitutions::new();
            let mut context = Context::new()
                .add_bindings(clone_bindings())
                .add_bindings(inst_clone_binding(Type::Base(Int)));

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)));
            println!("Result pretty: {}", unification.map(|a| a.to_string_type(&context, 0)).unwrap_or("Error".to_string()));
        }

        #[test]
        fn different_vars2() {
            // Arrange
            let t1 = TypeVar("U".to_string());
            let t2 = Type::qualified(
                vec![Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("T".to_string())] }],
                TypeVar("T".to_string()),
            );

            let mut substitutions = Substitutions::new();
            let mut context = Context::new()
                .add_bindings(clone_bindings())
                .add_bindings(inst_clone_binding(Base(Int)));

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)));
            println!("Result pretty: {}", unification.map(|a| a.to_string_type(&context, 0)).unwrap_or("Error".to_string()));
        }

        #[test]
        fn vec() {
            // Arrange
            let t1 = Type::qualified(
                vec![Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("T".to_string())] }],
                TypeVar("T".to_string()),
            );
            let t2 = TypeApp(Box::new(TypeVar("Vec".to_string())), Box::new(Type::Base(Int)));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new()
                .add_bindings(clone_bindings())
                .add_bindings(inst_clone_binding(Base(Int)))
                .add_bindings(vec_bindings());

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)));
            println!("Result pretty: {}", unification.map(|a| a.to_string_type(&context, 0)).unwrap_or("Error".to_string()));
        }

        #[test]
        fn vec_inner_wrong() {
            // Arrange
            let t1 = Type::qualified(
                vec![Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("T".to_string())] }],
                TypeVar("T".to_string()),
            );
            let t2 = TypeApp(Box::new(TypeVar("Vec".to_string())), Box::new(Type::Base(Bool)));

            let mut substitutions = Substitutions::new();
            let mut context = Context::new()
                .add_bindings(clone_bindings())
                .add_bindings(inst_clone_binding(Base(Int)))
                .add_bindings(vec_bindings());

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Err(..)));
            println!("Result pretty: {}", unification.map(|a| a.to_string_type(&context, 0)).unwrap_or("Error".to_string()));
        }

        #[test]
        fn vec_constructor_only() {
            // Arrange
            let t1 = Type::qualified(
                vec![Constraint { ident: "Clone".to_string(), vars: vec![TypeVar("T".to_string())] }],
                TypeVar("T".to_string()),
            );
            let t2 = TypeVar("Vec".to_string());

            let mut substitutions = Substitutions::new();
            let mut context = Context::new()
                .add_bindings(clone_bindings())
                .add_bindings(inst_clone_binding(Base(Int)))
                .add_bindings(vec_bindings());

            // Act
            println!("Unify: {} ⊔ {}", t1.to_string_type(&context, 0), t2.to_string_type(&context, 0));
            let unification = unify(&mut context, &mut substitutions, t1, t2, vec![], vec![]);

            // Assert
            println!();
            println!("Substitutions: {:?}", substitutions);
            println!("Result of unification: {:?}", unification);
            assert!(matches!(unification, Ok(..)));
            println!("Result pretty: {}", unification.map(|a| a.to_string_type(&context, 0)).unwrap_or("Error".to_string()));
        }
    }
}

