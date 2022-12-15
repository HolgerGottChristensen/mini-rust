use syn::{parse_quote, parse_str, Path};
use syn::punctuated::Punctuated;
use mini_ir::{Kind, Term};
use mini_ir::Kind::{KindArrow, KindStar};
use crate::expr::MiniExprPath;
use crate::item::{MiniFn, MiniFnArg};
use crate::mini_expr::MiniExpr;
use crate::mini_generics::{MiniGenerics, MiniTypeParam};
use crate::mini_ident::{MiniIdent, MiniRecIdent};
use crate::mini_item::MiniItem;
use crate::mini_stmt::MiniStmt;
use crate::stmt::MiniBlock;

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


pub fn term_to_mini_rust(term: Term) -> MiniStmt {
    match term {
        Term::TermVar(x) => {MiniStmt::Expr(MiniExpr::Path(MiniExprPath { qself: None, path: parse_str(&x).unwrap() }))}
        Term::TermAbs(x, ty, term) => {
            let inner = term_to_mini_rust(*term);

            if let MiniStmt::Item(MiniItem::Fn(MiniFn {
                fn_token,
                ident,
                generics,
                paren_token,
                inputs,
                return_type,
                block
            })) = inner {
                MiniStmt::Item(MiniItem::Fn(
                    MiniFn {
                        fn_token: Default::default(),
                        ident,
                        generics,
                        paren_token,
                        inputs: Punctuated::from_iter(inputs.into_iter().chain([
                            MiniFnArg::Typed {
                                pat: MiniIdent::new(&x),
                                colon_token: Default::default(),
                                ty: Box::new(type_to_mini_rust(ty))
                            }
                        ])),
                        return_type: None,
                        block
                    }
                ))
            } else {
                MiniStmt::Item(MiniItem::Fn(
                    MiniFn {
                        fn_token: Default::default(),
                        ident: MiniIdent::new("test"),
                        generics: Default::default(),
                        paren_token: Default::default(),
                        inputs: Punctuated::from_iter([
                            MiniFnArg::Typed {
                                pat: MiniIdent::new(&x),
                                colon_token: Default::default(),
                                ty: Box::new(type_to_mini_rust(ty))
                            }
                        ]),
                        return_type: None,
                        block: Ok(Box::new(MiniBlock { brace_token: Default::default(), stmts: vec![
                            inner
                        ] }))
                    }
                ))
            }


        }
        Term::TermApp(_, _) => {todo!()}
        Term::TermTypeAbs(x, knd, term) => {
            let inner = term_to_mini_rust(*term);

            if let MiniStmt::Item(MiniItem::Fn(MiniFn {
               fn_token,
               ident,
               generics: MiniGenerics {
                   params, ..
               },
               paren_token,
               inputs,
               return_type,
               block
           })) = inner {
                MiniStmt::Item(MiniItem::Fn(
                    MiniFn {
                        fn_token: Default::default(),
                        ident,
                        generics: MiniGenerics {
                            lt_token: Some(Default::default()),
                            params: Punctuated::from_iter(params.into_iter().chain(
                                [parse_str::<MiniTypeParam>("X").unwrap()]
                            )),
                            gt_token: Some(Default::default()),
                            where_clause: None
                        },
                        paren_token,
                        inputs,
                        return_type: None,
                        block
                    }
                ))
            } else {
                MiniStmt::Item(MiniItem::Fn(
                    MiniFn {
                        fn_token: Default::default(),
                        ident: MiniIdent::new("test"),
                        generics: MiniGenerics {
                            lt_token: Some(Default::default()),
                            params: Punctuated::from_iter([parse_str::<MiniTypeParam>("X").unwrap()]),
                            gt_token: Some(Default::default()),
                            where_clause: None
                        },
                        paren_token: Default::default(),
                        inputs: Default::default(),
                        return_type: None,
                        block: Ok(Box::new(MiniBlock { brace_token: Default::default(), stmts: vec![
                            inner
                        ] }))
                    }
                ))
            }
        }
        Term::TermTypeApp(_, _) => {todo!()}
        _ => unimplemented!()
    }
}

pub fn type_to_mini_rust(ty: mini_ir::Type) -> syn::Type {
    parse_quote!(i64)
}

pub fn to_string_kind(k: Kind) -> String {
    match k {
        Kind::KindStar => "X".to_string(),
        Kind::KindArrow(k1, k2) => {
            format!("{}, {}", to_string_atomic(*k1), to_string_kind(*k2))
        }
    }
}

fn to_string_atomic(k: Kind) -> String {
    match k {
        Kind::KindStar => "X".to_string(),
        Kind::KindArrow(k1, k2) => format!("X<{}>", to_string_kind(*k2)),
    }
}

pub fn kind_to_mini_rust(name: &str, kind: Kind) -> String {
    match kind {
        // (*)
        KindStar => {
            format!("{}", name)
        }
        // (k1 => k2)
        KindArrow(k1, k2)=> {
            let mut lhs = *k2;
            let mut acc = vec![kind_to_mini_rust(name, *k1)];

            while let KindArrow(k3, k4) = lhs {
                acc.push(kind_to_mini_rust(name, *k3));

                lhs = *k4;
            }

            format!("{}<{}>", name, acc.join(", "))
        }
    }
}

mod tests {
    use mini_ir::Kind::KindStar;
    use mini_ir::{Kind, Term};
    use crate::util::{chi, kind_to_mini_rust, term_to_mini_rust, to_string_atomic, to_string_kind};

    #[test]
    fn test_kind() {
        println!("{}", chi(Kind::arrow(KindStar, KindStar)));
        println!("{}", kind_to_mini_rust("X", Kind::arrow(KindStar, KindStar)));
        println!("{}", kind_to_mini_rust("X", Kind::arrow(Kind::arrow(KindStar, KindStar), KindStar)));
        println!("{}", kind_to_mini_rust("X", Kind::arrow(KindStar, Kind::arrow(KindStar, KindStar))));
        println!("{}", chi(Kind::arrow(KindStar, Kind::arrow(KindStar, KindStar))));
        println!("{}", kind_to_mini_rust("X", Kind::arrow(KindStar, Kind::arrow(KindStar, Kind::arrow(KindStar, Kind::arrow(Kind::arrow(KindStar, KindStar), Kind::arrow(KindStar, KindStar)))))));


        //let t = kind_to_mini_rust("X", Kind::arrow(KindStar, KindStar));
        //println!("{:?}", to_string_atomic(Kind::arrow(KindStar, KindStar)));
        //println!("{:?}", to_string_atomic(Kind::arrow(Kind::arrow(KindStar, KindStar), KindStar)));
        //let t = kind_to_mini_rust("X", Kind::arrow(Kind::arrow(KindStar, KindStar), KindStar));
        //println!("{:?}", t.0[0].print());
        //let t = kind_to_mini_rust("X", Kind::arrow(KindStar, Kind::arrow(KindStar, KindStar)));
        //println!("{:?}", t.0);
    }

    #[test]
    fn test() {

        let term = Term::TermTypeAbs(
            "T".to_string(),
            KindStar,
            Box::new(Term::TermAbs(
                "arg1".to_string(),
                mini_ir::Type::TypeVar("A".to_string()),
                Box::new(Term::TermAbs(
                    "arg2".to_string(),
                    mini_ir::Type::TypeVar("B".to_string()),
                    Box::new(Term::TermVar("arg2".to_string()))
                ))
            ))
        );

        let f = term_to_mini_rust(term);

        println!("{:#?}", f);
    }
}