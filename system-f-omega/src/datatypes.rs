use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use crate::{Context, get_color, pick_fresh_name};
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

#[derive(Clone, Debug, PartialEq)]
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
    /// {∃X::Kind, Type}
    Existential(String, Kind, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_type(&Context::new(), 0))
    }
}

impl Type {
    pub fn to_string_type(&self, context: &Context, color: u32) -> String {
        match self {
            Type::TypeAbs(tyX,knK1,tyT2) => {
                // Todo: Fix will not work with pick fresh name. We already require unique names for everything.
                let (new_context, name) = pick_fresh_name(context, tyX.clone());

                format!("λ{}::{}. {}", name, knK1, tyT2.to_string_type(&new_context, color))
            }
            Type::TypeAll(tyX, knK1, tyT2) => {
                let (new_context, name) = pick_fresh_name(context, tyX.clone());

                format!("∀{}::{}. {}", name, knK1, tyT2.to_string_type(&new_context, color))
            }
            Type::Recursive(tyX, knK1, tyT2) => {
                let (new_context, name) = pick_fresh_name(context, tyX.clone());

                format!("μ{}::{}. {}", name, knK1, tyT2.to_string_type(&new_context, color))
            }
            Type::Existential(x, knK1, tyT2) => {
                let (new_context, name) = pick_fresh_name(context, x.clone());

                format!("{}∃{}::{}, {}{}", get_color(color, "{"), name, knK1, tyT2.to_string_type(&new_context, color + 1), get_color(color, "}"))
            }
            t => t.to_string_arrow(context, color),
        }
    }

    fn to_string_arrow(&self, context: &Context, color: u32) -> String {
        match self {
            Type::TypeArrow(tyT1,tyT2) => {
                format!("{} -> {}", tyT1.to_string_app_type(context, color), tyT2.to_string_arrow(context, color))
            }
            t => t.to_string_app_type(context, color),
        }
    }

    fn to_string_app_type(&self, context: &Context, color: u32) -> String {
        match self {
            Type::TypeApp(tyT1,tyT2) => {
                format!("{} [{}]", tyT1.to_string_app_type(context, color), tyT2.to_string_atomic(context, color))
            }
            t => t.to_string_atomic(context, color),
        }
    }

    fn to_string_atomic(&self, context: &Context, color: u32) -> String {
        match self {
            Type::TypeVar(x) => {
                format!("{}", x)
            }
            Type::Tuple(types) => {
                let mut r = types.iter().map(|a| a.to_string_type(context, color + 1)).collect::<Vec<_>>().join(", ");
                format!("{}{}{}", get_color(color, "{"), r, get_color(color, "}"), )
            }
            Type::Record(terms) => {
                let mut r = terms.iter().map(|(name, term)| {
                    let mut s = name.clone();
                    s.push_str(": ");
                    s.push_str(&term.to_string_type(context, color + 1));
                    s
                }).collect::<Vec<_>>().join(", ");
                format!("{}{}{}", get_color(color, "⟨"), r, get_color(color, "⟩")) // We use angle brackets which are non-standard to differentiate between records and tuples.
            },
            Type::Variants(terms) => {
                let mut r = terms.iter().map(|(name, term)| {
                    let mut s = name.clone();
                    s.push_str(" = ");
                    s.push_str(&term.to_string_type(context, color + 1));
                    s
                }).collect::<Vec<_>>().join(", ");
                format!("{}{}{}", get_color(color, "<"), r, get_color(color, ">"))
            },
            Type::Base(b) => format!("{:?}", b),
            Type::Reference(b) => format!("&{}", b.to_string_type(context, color)),
            t => format!("{}{}{}", get_color(color, "("), t.to_string_type(context, color + 1), get_color(color, ")"))
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    /// x, y, z ...
    TermVar(String),
    /// lambda x: Type. Term
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
    /// case Term of <label=x> => Term | <label2=x2> => Term | ...
    Case(Box<Term>, HashMap<String, (String, Term)>),
    /// fold \[Type\]
    Fold(Type),
    /// unfold \[Type\]
    UnFold(Type),
    /// {* Type,Term} as Type
    Pack(Type, Box<Term>, Type),
    /// let {X, x} = Term in Term
    UnPack(String, String, Box<Term>, Box<Term>),
    /// Term as Type
    Ascribe(Box<Term>, Type),
    /// The below is a bit special so I will try to explain a bit.
    /// We need some way of introducing types into the context for a scope.
    /// One way it could happen would be having TermTypeApp and TermTypeAbs
    /// but this would result in a Kind to be brought into scope, but we specifically need
    /// a type and not a kind in scope.
    /// The only ways I see of bringing types into scope is manually doing it before the
    /// context is even used by modifying the context before passing it to type_of and co.
    /// This would mean a preprocessing step to collect all types in the scopes. Not fun.
    /// The other way I see is using TermApp and TermVar, but these require a Term and we do
    /// not have a term for example from a struct definition.
    ///
    /// What we instead will allow with this statement is that we can bring a type into scope
    /// even without it having a term.
    ///
    /// Define should really only be used at a tool for better ascriptions, and not as a replacement
    /// for abstraction and application.
    ///
    /// define x as Type in Term
    Define(String, Type, Box<Term>),
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_term(&Context::new(), 0))
    }
}

impl Term {
    pub fn sequence(self, other: Term) -> Term {
        Term::TermApp(
            Box::new(Term::TermAbs(
                "_".to_string(),
                Type::Base(BaseType::Unit),
                Box::new(other)
            )),
            Box::new(self),
        )
    }

    fn to_string_atomic(&self, context: &Context, color: u32) -> String {
        match self {
            Term::TermVar(x) => {
                //colorize_string(format!("<magenta>{}</>", x))
                format!("{}", x)
            },
            Term::True => colorize_string("<green>true</>"),
            Term::False => colorize_string("<green>false</>"),
            Term::Unit => colorize_string("<green>unit</>"),
            Term::Float(f) => colorize_string(format!("<green>{:?}</>", f)),
            Term::Integer(i) => colorize_string(format!("<green>{}</>", i)),

            Term::Ascribe(t, ty) => format!("{}{}{} as {}", get_color(color, "("), t.to_string_term(context, color + 1), get_color(color, ")"), ty.to_string_type(context, color)),
            Term::Define(x, ty, term) => format!("define {}{} = {}{} in {}", get_color(color, "("), x, ty.to_string_type(context, color + 1), get_color(color, ")"), term.to_string_term(context, color)),
            Term::Fold(t) => format!("fold {}{}{}", get_color(color, "["), t.to_string_type(context, color + 1), get_color(color, "]")),
            Term::UnFold(t) => format!("unfold {}{}{}", get_color(color, "["), t.to_string_type(context, color + 1), get_color(color, "]")),
            Term::Pack(t1, term1, t2) =>
                format!("{}{}* {}, {}{} as {}{}", get_color(color, "("), get_color(color + 1, "{"), t1.to_string_type(context, color + 2), term1.to_string_term(context, color + 2), get_color(color + 1, "}"), t2.to_string_type(context, color + 1), get_color(color, ")")),
            Term::UnPack(x1, x2, term1, term2) =>
                format!("let {} {}, {} {} = {} in {}", get_color(color, "{"), x1, x2, get_color(color, "}"), term1.to_string_term(context, color), term2.to_string_term(context, color)),
            Term::If(t1, t2, t3) =>
                format!("if {} then {} else {}", t1.to_string_term(context, color), t2.to_string_term(context, color), t3.to_string_term(context, color)),
            Term::Let(x, t1, t2) =>
                format!("let {} = {}{}{} in {}{}{}", x, get_color(color, "("), t1.to_string_term(context, color + 1), get_color(color, ")"),  get_color(color, "("), t2.to_string_term(context, color + 1), get_color(color, ")")),
            Term::Tuple(terms) => {
                let mut r = terms.iter().map(|a| a.to_string_term(context, color + 1)).collect::<Vec<_>>().join(", ");
                format!("{}{}{}", get_color(color, "{"), r, get_color(color, "}"))
            },
            Term::Record(terms) => {
                let mut r = terms.iter().map(|(name, term)| {
                    let mut s = name.clone();
                    s.push_str(" = ");
                    s.push_str(&term.to_string_term(context, color + 1));
                    s
                }).collect::<Vec<_>>().join(", ");
                format!("{}{}{}", get_color(color, "{"), r, get_color(color, "}"))
            },
            Term::TupleProjection(t1, index) => {
                format!("{}.{}", t1.to_string_term(context, color), index)
            }
            Term::RecordProjection(t1, label) => {
                format!("{}.{}", t1.to_string_term(context, color), label)
            }
            Term::Tagging(label, term, t1) => {
                format!("{}{}={}{} as {}", get_color(color, "<"), label, term.to_string_term(context, color + 1), get_color(color, ">"), t1.to_string_type(context, color))
            }
            Term::Case(term, cases) => {
                let cases_string = cases.iter().map(|(label, (x, term2))| {
                    format!("{}{}={}{} => {}", get_color(color, "<"), label, x, get_color(color, ">"), term2.to_string_term(context, color))
                }).collect::<Vec<_>>().join(" | ");

                format!("case {}{}{} of {}", get_color(color, "("), term.to_string_term(context, color + 1), get_color(color, ")"), cases_string)
            }
            Term::Reference(b) => format!("&{}", b.to_string_term(context, color)),
            t => format!("{}{}{}", get_color(color, "("), t.to_string_term(context, color + 1), get_color(color, ")"))
        }
    }

    fn to_string_term(&self, context: &Context, color: u32) -> String {
        match self {
            Term::TermAbs(x,tyT1,t2) => {
                let (new_context, name) = pick_fresh_name(context, x.clone());

                format!("λ{}: {}. {}", name, tyT1.to_string_type(context, color), t2.to_string_term(&new_context, color))
            }
            Term::TermTypeAbs(x,knK,t) => {
                let (new_context, name) = pick_fresh_name(context, x.clone());

                format!("λ{}::{}. {}", name, knK, t.to_string_term(&new_context, color))
            }
            t => t.to_string_term_app(context, color),
        }
    }

    fn to_string_term_app(&self, context: &Context, color: u32) -> String {
        match self {
            Term::TermApp(t1, t2) => {
                format!("{} {}", t1.to_string_term_app(context, color), t2.to_string_atomic(context, color))
            }
            Term::TermTypeApp(t1, t2) => {
                format!("{} {}{}{}", t1.to_string_term_app(context, color), get_color(color, "["), t2.to_string_type(context, color + 1), get_color(color, "]"))
            }
            t => format!("{}", t.to_string_atomic(context, color)),
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