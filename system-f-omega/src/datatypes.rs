use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use crate::{Context, pick_fresh_name};
use paris::formatter::colorize_string;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
pub enum Kind {
    /// *
    KindStar,
    /// Kind -> Kind
    KindArrow(Box<Kind>, Box<Kind>),
}

impl Display for Kind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::KindStar => write!(f, "{}", colorize_string("<bright-blue><b>*</>")),
            Kind::KindArrow(k1, k2) => {
                let colored = format!("{} <bright-blue><b>=></> {}", k1, k2);
                write!(f, "{}", colorize_string(colored))
            },
        }
    }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum BaseType {
    Int,
    Bool,
    Unit,
    Float,
}

impl Debug for BaseType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseType::Int => write!(f, "Int"),
            BaseType::Bool => write!(f, "Bool"),
            BaseType::Unit => write!(f, "Unit"),
            BaseType::Float => write!(f, "Float"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    /// X, Y, Z
    TypeVar(String),
    /// Type -> Type
    TypeArrow(Box<Type>, Box<Type>),
    /// λ X::Kind. Type
    TypeAbs(String, Kind, Box<Type>),
    /// Type Type
    TypeApp(Box<Type>, Box<Type>),
    /// ∀ X::Kind. Type
    TypeAll(String, Kind, Box<Type>),
    /// Bool, Int, ...
    Base(BaseType),
    /// & Type
    Reference(Box<Type>),
    /// {Type, Type, ...}
    Tuple(Vec<Type>),
    /// {l1=Type, l2=Type, ...}
    Record(HashMap<String, Type>),
    /// <l1=Type, l2=Type, ...>
    Variants(HashMap<String, Type>),
    /// μ X::Kind. Type
    Recursive(String, Kind, Box<Type>),
    /// {∃X::Kind,Type}
    Existential(String, Kind, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_type(&Context::new()))
    }
}

impl Type {

    fn to_string_type(&self, context: &Context) -> String {
        match self {
            Type::TypeAbs(tyX,knK1,tyT2) => {
                // Todo: Fix will not work with pick fresh name. We already require unique names for everything.
                let (new_context, name) = pick_fresh_name(context, tyX.clone());

                format!("λ{}::{}. {}", name, knK1, tyT2.to_string_type(&new_context))
            }
            Type::TypeAll(tyX, knK1, tyT2) => {
                let (new_context, name) = pick_fresh_name(context, tyX.clone());

                format!("∀{}::{}. {}", name, knK1, tyT2.to_string_type(&new_context))
            }
            Type::Recursive(tyX, knK1, tyT2) => {
                let (new_context, name) = pick_fresh_name(context, tyX.clone());

                format!("μ{}::{}. {}", name, knK1, tyT2.to_string_type(&new_context))
            }
            Type::Existential(x, knK1, tyT2) => {
                let (new_context, name) = pick_fresh_name(context, x.clone());

                format!("{{∃{}::{}, {}}}", name, knK1, tyT2.to_string_type(&new_context))
            }
            t => t.to_string_arrow(context),
        }
    }

    fn to_string_arrow(&self, context: &Context) -> String {
        match self {
            Type::TypeArrow(tyT1,tyT2) => {
                format!("{} -> {}", tyT1.to_string_app_type(context), tyT2.to_string_arrow(context))
            }
            t => t.to_string_app_type(context),
        }
    }

    fn to_string_app_type(&self, context: &Context) -> String {
        match self {
            Type::TypeApp(tyT1,tyT2) => {
                format!("{} {}", tyT1.to_string_app_type(context), tyT2.to_string_atomic(context))
            }
            t => t.to_string_atomic(context),
        }
    }

    fn to_string_atomic(&self, context: &Context) -> String {
        match self {
            Type::TypeVar(x) => {
                format!("{}", x)
            }
            Type::Tuple(types) => {
                let mut r = types.iter().map(|a| a.to_string_type(context)).collect::<Vec<_>>().join(", ");
                format!("{{{}}}", r)
            }
            Type::Record(terms) => {
                let mut r = terms.iter().map(|(name, term)| {
                    let mut s = name.clone();
                    s.push_str(": ");
                    s.push_str(&term.to_string_type(context));
                    s
                }).collect::<Vec<_>>().join(", ");
                format!("{{{}}}", r)
            },
            Type::Variants(terms) => {
                let mut r = terms.iter().map(|(name, term)| {
                    let mut s = name.clone();
                    s.push_str(" = ");
                    s.push_str(&term.to_string_type(context));
                    s
                }).collect::<Vec<_>>().join(", ");
                format!("<{}>", r)
            },
            Type::Base(b) => format!("{:?}", b),
            t => format!("({})", t.to_string_type(context))
        }
    }
}

#[derive(Clone)]
pub enum Term {
    /// x, y, z ...
    TermVar(String),
    /// lambda x:Type. Term
    TermAbs(String, Type, Box<Term>),
    /// Term Term
    TermApp(Box<Term>, Box<Term>),
    /// lambda X::Kind. Term
    TermTypeAbs(String, Kind, Box<Term>),
    /// Term \[Type\]
    TermTypeApp(Box<Term>, Type),
    /// true
    True,
    /// false
    False,
    /// 0, 1, 2 ...
    Integer(i64),
    /// 0.121312, 3.14 ...
    Float(f64),
    /// unit
    Unit,
    /// & Term
    Reference(Box<Term>),
    /// If Term then Term else Term
    If(Box<Term>, Box<Term>, Box<Term>),
    /// let x = Term in Term
    Let(String, Box<Term>, Box<Term>),
    /// {Term, Term, ...}
    Tuple(Vec<Term>),
    /// Term.i
    TupleProjection(Box<Term>, usize),
    /// {l1=Term, l2=Term, ...}
    Record(HashMap<String, Term>),
    /// Term.l
    RecordProjection(Box<Term>, String),
    /// <l=Term> as Type
    Tagging(String, Box<Term>, Type),
    /// case Term of <label=x> => Term | <label=x> => Term | ...
    Case(Box<Term>, HashMap<String, (String, Term)>),
    /// fold \[Type\]
    Fold(Type),
    /// unfold \[Type\]
    UnFold(Type),
    /// {* Type,Term} as Type
    Pack(Type, Box<Term>, Type),
    /// let {X, x} = Term in Term
    UnPack(String, String, Box<Term>, Box<Term>),
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_term(&Context::new()))
    }
}

impl Term {
    fn to_string_atomic(&self, context: &Context) -> String {
        match self {
            Term::TermVar(x) => {
                format!("{}", x)
            },
            Term::True => "true".to_string(),
            Term::False => "false".to_string(),
            Term::Unit => "unit".to_string(),
            Term::Fold(t) => format!("fold [{}]", t.to_string_type(context)),
            Term::UnFold(t) => format!("unfold [{}]", t.to_string_type(context)),
            Term::Pack(t1, term1, t2) =>
                format!("({{* {}, {}}} as {})", t1.to_string_type(context), term1.to_string_term(context), t2.to_string_type(context)),
            Term::UnPack(x1, x2, term1, term2) =>
                format!("let {{ {}, {} }} = {} in {}", x1, x2, term1.to_string_term(context), term2.to_string_term(context)),
            Term::Float(f) => format!("{:?}", f),
            Term::Integer(i) => format!("{}", i),
            Term::If(t1, t2, t3) =>
                format!("if {} then {} else {}", t1.to_string_term(context), t2.to_string_term(context), t3.to_string_term(context)),
            Term::Let(x, t1, t2) =>
                format!("let {} = {} in {}", x, t1.to_string_term(context), t2.to_string_term(context)),
            Term::Tuple(terms) => {
                let mut r = terms.iter().map(|a| a.to_string_term(context)).collect::<Vec<_>>().join(", ");
                format!("{{{}}}", r)
            },
            Term::Record(terms) => {
                let mut r = terms.iter().map(|(name, term)| {
                    let mut s = name.clone();
                    s.push_str(" = ");
                    s.push_str(&term.to_string_term(context));
                    s
                }).collect::<Vec<_>>().join(", ");
                format!("{{{}}}", r)
            },
            Term::TupleProjection(t1, index) => {
                format!("{}.{}", t1.to_string_term(context), index)
            }
            Term::RecordProjection(t1, label) => {
                format!("{}.{}", t1.to_string_term(context), label)
            }
            Term::Tagging(label, term, t1) => {
                format!("<{}={}> as {}", label, term.to_string_term(context), t1.to_string_type(context))
            }
            Term::Case(term, cases) => {
                let cases_string = cases.iter().map(|(label, (x, term2))| {
                    format!("<{}={}> => {}", label, x, term2.to_string_term(context))
                }).collect::<Vec<_>>().join(" | ");

                format!("case ({}) of {}", term.to_string_term(context), cases_string)
            }
            t => format!("({})", t.to_string_term(context))
        }
    }

    fn to_string_term(&self, context: &Context) -> String {
        match self {
            Term::TermAbs(x,tyT1,t2) => {
                let (new_context, name) = pick_fresh_name(context, x.clone());

                format!("λ{}: {}. {}", name, tyT1.to_string_type(context), t2.to_string_term(&new_context))
            }
            Term::TermTypeAbs(x,knK,t) => {
                let (new_context, name) = pick_fresh_name(context, x.clone());

                format!("λ{}::{}. {}", name, knK, t.to_string_term(&new_context))
            }
            t => t.to_string_term_app(context),
        }
    }

    fn to_string_term_app(&self, context: &Context) -> String {
        match self {
            Term::TermApp(t1, t2) => {
                format!("{} {}", t1.to_string_term_app(context), t2.to_string_atomic(context))
            }
            Term::TermTypeApp(t1, t2) => {
                format!("{} [{}]", t1.to_string_term_app(context), t2.to_string_type(context))
            }
            t => t.to_string_atomic(context),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Binding {
    NameBinding(String),
    VarBinding(String, Type),
    TyVarBinding(String, Kind)
}

impl PartialEq<Binding> for &String{
    fn eq(&self, other: &Binding) -> bool {
        match other {
            Binding::NameBinding(s) |
            Binding::VarBinding(s, _) |
            Binding::TyVarBinding(s, _) => s == *self,
        }
    }
}