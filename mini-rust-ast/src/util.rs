use mini_ir::Term;

pub fn replace_inner(term: Term, with: Term) -> Result<Term, String> {
    match term {
        Term::Define(x, ty, inner) => {
            Ok(Term::Define(x, ty, Box::new(replace_inner(*inner, with)?)))
        }
        Term::Let(a, ty, inner) => {
            Ok(Term::Let(a, ty, Box::new(replace_inner(*inner, with)?)))
        }
        Term::Replacement => {
            Ok(with)
        }
        Term::Seq(term1, term2) => {
            Ok(Term::Seq(term1, Box::new(replace_inner(*term2, with)?)))
        }
        Term::TermTypeAbs(s, k, inner) => {
            Ok(Term::TermTypeAbs(s, k, Box::new(replace_inner(*inner, with)?)))
        }
        Term::Class {
            constraints,
            name,
            vars,
            declarations,
            default_implementations,
            continuation
        } => {
            Ok(Term::Class {
                constraints,
                name,
                vars,
                declarations,
                default_implementations,
                continuation: Box::new(replace_inner(*continuation, with)?)
            })
        }
        Term::Instance {
            constraints,
            class_name,
            ty,
            implementations,
            continuation
        } => {
            Ok(Term::Instance {
                constraints,
                class_name,
                ty,
                implementations,
                continuation: Box::new(replace_inner(*continuation, with)?)
            })
        }
        a => Err("Expressions need to be the last in the block".to_string())
    }
}