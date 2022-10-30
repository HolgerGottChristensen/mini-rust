use mini_ir::Term;

pub fn replace_inner(term: Term, with: Term) -> Term {
    match term {
        Term::Define(x, ty, inner) => {
            Term::Define(x, ty, Box::new(replace_inner(*inner, with)))
        }
        Term::Let(a, ty, inner) => {
            Term::Let(a, ty, Box::new(replace_inner(*inner, with)))
        }
        Term::Unit => {
            with
        }
        Term::Seq(term1, term2) => {
            Term::Seq(term1, Box::new(replace_inner(*term2, with)))
        }
        Term::TermTypeAbs(s, k, inner) => {
            Term::TermTypeAbs(s, k, Box::new(replace_inner(*inner, with)))
        }
        Term::Class {
            constraints,
            name,
            vars,
            declarations,
            default_implementations,
            continuation
        } => {
            Term::Class {
                constraints,
                name,
                vars,
                declarations,
                default_implementations,
                continuation: Box::new(replace_inner(*continuation, with))
            }
        }
        Term::Instance {
            constraints,
            class_name,
            ty,
            implementations,
            continuation
        } => {
            Term::Instance {
                constraints,
                class_name,
                ty,
                implementations,
                continuation: Box::new(replace_inner(*continuation, with))
            }
        }
        a => panic!("Expressions need to be the last in the block")
    }
}