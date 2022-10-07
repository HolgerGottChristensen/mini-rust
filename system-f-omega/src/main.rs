mod datatypes;
mod context_management;
mod eval;

pub use datatypes::*;
pub use context_management::*;
pub use eval::*;
use system_f_omega_macros::my_macro;
use crate::Kind::KindStar;
use crate::Term::TermTypeAbs;

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

    /*let term = TermTypeAbs(
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

    println!("Term: {}", term);
    println!("Type: {}", type_of(&context, term));*/

    let term = Term::TermAbs(
        "x".to_string(),
        Type::Bool,
        Box::new(Term::If(
            Box::new(Term::TermVar("x".to_string())),
            Box::new(Term::Integer(42)),
            Box::new(Term::Integer(420)),
        ))
    );

    println!("Term: {}", term);
    println!("Type: {}", type_of(&context, term));

    //my_macro!(((* => *) => *) => *)
    //my_macro!(lambda X:*.)
}
