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
        a => Err("Expressions need to be the last in the block".to_string())
    }
}