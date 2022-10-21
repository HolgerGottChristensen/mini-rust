extern crate core;

mod datatypes;
mod context_management;
mod eval;
mod free_variables;
mod dependency_graph;

use std::collections::HashMap;
pub use datatypes::*;
pub use context_management::*;
pub use eval::*;
use crate::BaseType::Int;
use crate::Kind::KindStar;
use crate::Term::{TermAbs, TermApp, TermTypeAbs, TermVar};
use crate::Type::{Existential, TypeArrow, TypeVar};

#[test]
fn overload() {
    let context = Context::new();

    fn eq_type(ty: Type) -> Type {
        Type::TypeArrow(
            Box::new(ty.clone()),
            Box::new(TypeArrow(
                Box::new(ty.clone()),
                Box::new(Type::Base(BaseType::Bool))
            )),
        )
    }

    fn eq_term(ty: Type) -> Term {
        Term::TermAbs("x".to_string(), ty.clone(), Box::new(
            Term::TermAbs("y".to_string(), ty.clone(), Box::new(
                Term::True
            ))
        ))
    }

    let over_type = Type::TypeAll("X".to_string(), KindStar, Box::new(
        eq_type(TypeVar("X".to_string()))
    ));

    let over = Term::Overload("eq".to_string(), over_type, Box::new(
        Term::Instance("eq".to_string(), eq_type(Type::Base(BaseType::Int)), Box::new(eq_term(Type::Base(BaseType::Int))), Box::new(Term::Unit))
    ));

    println!("{}", &over);
    println!("{}", type_of(&context, over));
    println!();
}

#[test]
fn main() {
    /*let context = Context::new();
    let context = add_binding(&context, "X".to_string(), Binding::TyVarBinding(KindStar));
    let ty = type_of(
        &context,
        Term::TermAbs(
            "".to_string(),
            "x".to_string(),
            Type::TypeVar(0, 1),
            Box::new(Term::TermVar("".to_string(),1, 2)))
    );

    println!("{}", ty);*/


    /*let context = Context::new();
    let context = add_binding(&context, "X".to_string(), Binding::TyVarBinding(KindStar));
    let kind = kind_of(&context, Type::TypeVar(0, 1));

    println!("{}", kind);*/

    let context = Context::new();

    /*let term = Term::TermAbs(
        "x".to_string(),
        Type::Base(BaseType::Bool),
        Box::new(Term::If(
            Box::new(Term::TermVar("x".to_string())),
            Box::new(Term::Float(42.0)),
            Box::new(Term::Float(22.0)),
        ))
    );*/

    /*let term = Term::TermAbs(
        "x".to_string(),
        Type::Base(BaseType::Bool),
        Box::new(Term::Let(
            "p".to_string(),
            Box::new(Term::TermVar("x".to_string())),
            Box::new(Term::TermVar("p".to_string())),
        ))
    );*/

    /*let term = Term::TermAbs(
        "x".to_string(),
        Type::Base(BaseType::Bool),
        Box::new(Term::TupleProjection(
            Box::new(Term::Tuple(vec![
                Term::False,
                Term::True,
                Term::Unit,
                Term::Integer(43),
            ])),
            3
        ))
    );*/

    /*let term = Term::TermAbs(
        "x".to_string(),
        Type::Base(BaseType::Bool),
        Box::new(Term::RecordProjection(
            Box::new(Term::Record(HashMap::from([
                ("p".to_string(), Term::True),
                ("q".to_string(), Term::Integer(42)),
                ("r".to_string(), Term::Unit),
            ]))),
            "q".to_string()
        ))
    );*/

    /*let term = Term::TermAbs(
        "x".to_string(),
        Type::Base(BaseType::Bool),
        Box::new(Term::Tagging(
            "p".to_string(),
            Box::new(Term::True),
            Type::Variants(HashMap::from([
                ("p".to_string(), Type::Base(BaseType::Bool)),
                ("q".to_string(), Type::Base(BaseType::Int)),
                ("r".to_string(), Type::Base(BaseType::Unit)),
            ]))
        ))
    );*/

    /*let term = Term::TermAbs(
        "x".to_string(),
        Type::Base(BaseType::Bool),
        Box::new(Term::Case(
            Box::new(Term::Tagging(
                "p".to_string(),
                Box::new(Term::True),
                Type::Variants(HashMap::from([
                    ("p".to_string(), Type::Base(BaseType::Bool)),
                    ("q".to_string(), Type::Base(BaseType::Int)),
                    ("r".to_string(), Type::Base(BaseType::Unit)),
                ]))
            )),
            HashMap::from([
                ("p".to_string(), ("x".to_string(), Term::True)),
                ("q".to_string(), ("y".to_string(), Term::False)),
                ("r".to_string(), ("z".to_string(), Term::True)),
            ])
        ))
    );*/

    // NatList = μX. <nil:Unit, cons:{Nat,X}>;
    // NLBody = <nil:Unit, cons:{Nat,NatList}>;
    // nil = fold [NatList] (<nil=unit> as NLBody);

    let nat_list = Type::Recursive(
        "X".to_string(),
        Kind::KindStar,
        Box::new(Type::Variants(HashMap::from([
            ("nil".to_string(), Type::Base(BaseType::Unit)),
            ("cons".to_string(), Type::Tuple(vec![Type::Base(BaseType::Int), Type::TypeVar("X".to_string())])),
        ])))
    );

    let nat_list_body = Type::Variants(HashMap::from([
        ("nil".to_string(), Type::Base(BaseType::Unit)),
        ("cons".to_string(), Type::Tuple(vec![Type::Base(BaseType::Int), nat_list.clone()])),
    ]));

    let nil = Term::TermApp(
        Box::new(Term::Fold(nat_list.clone())),
        Box::new(Term::Tagging(
            "nil".to_string(),
            Box::new(Term::Unit),
            nat_list_body.clone()
        ))
    );

    let cons = Term::TermAbs(
        "n".to_string(),
        Type::Base(BaseType::Int),
        Box::new(Term::TermAbs(
            "l".to_string(),
            nat_list.clone(),
            Box::new(Term::TermApp(
                Box::new(Term::Fold(nat_list.clone())),
                Box::new(Term::Tagging(
                    "cons".to_string(),
                    Box::new(Term::Tuple(vec![
                        Term::TermVar("n".to_string()),
                        Term::TermVar("l".to_string())
                    ])),
                    nat_list_body.clone()
                ))
            ))
        ))
    );

    let is_nil = TermAbs(
        "l".to_string(),
        nat_list.clone(),
        Box::new(Term::Case(
            Box::new(Term::TermApp(
                Box::new(Term::UnFold(nat_list.clone())),
                Box::new(Term::TermVar("l".to_string()))
            )),
            HashMap::from([
                ("nil".to_string(), ("u".to_string(), Term::True)),
                ("cons".to_string(), ("p".to_string(), Term::False)),
            ])
        ))
    );

    let head = TermAbs(
        "l".to_string(),
        nat_list.clone(),
        Box::new(Term::Case(
            Box::new(Term::TermApp(
                Box::new(Term::UnFold(nat_list.clone())),
                Box::new(Term::TermVar("l".to_string()))
            )),
            HashMap::from([
                ("nil".to_string(), ("u".to_string(), Term::Integer(0))),
                ("cons".to_string(), ("p".to_string(), Term::TupleProjection(Box::new(Term::TermVar("p".to_string())), 0))),
            ])
        ))
    );

    let id = TermTypeAbs(
        "X".to_string(),
        KindStar,
        Box::new(
            Term::TermAbs(
                "x".to_string(),
                Type::TypeVar("X".to_string()),
                Box::new(Term::TermVar("x".to_string()))
            )
        )
    );

    let double = TermTypeAbs(
        "X".to_string(),
        KindStar,
        Box::new(
            Term::TermAbs(
                "f".to_string(),
                Type::TypeArrow(
                    Box::new(Type::TypeVar("X".to_string())),
                    Box::new(Type::TypeVar("X".to_string())),
                ),
                Box::new(Term::TermAbs(
                    "a".to_string(),
                    Type::TypeVar("X".to_string()),
                    Box::new(Term::TermApp(
                        Box::new(Term::TermVar("f".to_string())),
                        Box::new(TermApp(
                            Box::new(Term::TermVar("f".to_string())),
                            Box::new(Term::TermVar("a".to_string()))
                        ))
                    ))
                ))
            )
        )
    );

    // p2 = {*Nat, 0} as {∃X,X};
    let p2 = Term::Pack(
        Type::Base(Int),
        Box::new(Term::Integer(0)),
        Existential(
            "X".to_string(),
            Kind::KindStar,
            Box::new(Type::TypeVar("X".to_string()))
        )
    );

    // p3 = {*Bool, true} as {∃X,X};
    let p3 = Term::Pack(
        Type::Base(BaseType::Bool),
        Box::new(Term::True),
        Existential(
            "X".to_string(),
            Kind::KindStar,
            Box::new(Type::TypeVar("X".to_string()))
        )
    );

    // p4 = {*Nat, {a=0, f=λx:Nat. succ(x)}} as {∃X, {a:X, f:X→Nat}}
    let p4 = Term::Pack(
        Type::Base(BaseType::Int),
        Box::new(Term::Record(HashMap::from([
            ("a".to_string(), Term::Integer(0)),
            ("f".to_string(), Term::TermAbs(
                "x".to_string(),
                Type::Base(BaseType::Int),
                Box::new(TermVar("x".to_string()))
            )),
        ]))),
        Type::Existential(
            "X".to_string(),
            Kind::KindStar,
            Box::new(Type::Record(HashMap::from([
                ("a".to_string(), TypeVar("X".to_string())),
                ("f".to_string(), Type::TypeArrow(
                    Box::new(Type::TypeVar("X".to_string())),
                    Box::new(Type::Base(BaseType::Int))
                )),
            ])))
        )
    );

    let hungry = Type::Recursive("A".to_string(), Kind::KindStar, Box::new(Type::TypeArrow(Box::new(Type::Base(BaseType::Int)), Box::new(TypeVar("A".to_string())))));

    let f = Term::Fix(Box::new(Term::TermAbs("f".to_string(), Type::TypeArrow(Box::new(Type::Base(BaseType::Int)), Box::new(hungry.clone())), Box::new(Term::TermVar("f".to_string())))));


    println!("NatList = {}", nat_list);
    println!("NLBody = {}", nat_list_body);
    println!();
    println!("nil = {}", &nil);
    println!("nil = {}", type_of(&context, nil));
    println!();
    println!("cons = {}", &cons);
    println!("cons = {}", type_of(&context, cons));
    println!();
    println!("is_nil = {}", &is_nil);
    println!("is_nil = {}", type_of(&context, is_nil));
    println!();
    println!("head = {}", &head);
    println!("head = {}", type_of(&context, head));
    println!();
    println!("id = {}", &id);
    println!("id = {}", type_of(&context, id.clone()));
    println!("id(applied) = {}", type_of(&context, Term::TermTypeApp(Box::new(id), Type::Base(BaseType::Int))));
    println!();
    println!("double = {}", &double);
    println!("double = {}", type_of(&context, double.clone()));
    println!("double(applied) = {}", type_of(&context, Term::TermTypeApp(Box::new(double.clone()), Type::Base(BaseType::Int))));
    println!("double(applied 2) = {}", type_of(&context, Term::TermTypeApp(Box::new(double), Type::TypeArrow(
        Box::new(Type::Base(BaseType::Int)),
        Box::new(Type::Base(BaseType::Int)),
    ))));
    println!();
    println!("p2 = {}", &p2);
    println!("p2 = {}", type_of(&context, p2));
    println!();
    println!("p3 = {}", &p3);
    println!("p3 = {}", type_of(&context, p3));
    println!();
    println!("p4 = {}", &p4);
    println!("p4 = {}", type_of(&context, p4));

    println!();
    println!("hungry = {}", &hungry);
    println!();
    println!("f = {}", &f);
    println!("f = {}", type_of(&context, f.clone()));
    println!("f(applied) = {}", Term::TermApp(Box::new(TermApp(Box::new(f.clone()), Box::new(Term::Integer(1)))), Box::new(Term::Integer(2))));
    //println!("f(applied) = {}", type_of(&context, Term::TermApp(Box::new(TermApp(Box::new(f), Box::new(Term::Integer(1)))), Box::new(Term::Integer(2)))));


    //println!("Term: {}", term);
    //println!("Type: {}", type_of(&context, term));
}
