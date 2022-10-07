use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use crate::{index_to_name, pick_fresh_name};
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

#[derive(Clone, Debug)]
pub enum Type {
    /// X, Y, Z
    TypeVar(
        /// specifies the distance to the variable’s binder
        i64,
        /// specifies the expected total size of the context
        i64
    ),
    /// Type -> Type
    TypeArrow(Box<Type>, Box<Type>),
    /// lambda X::Kind. Type
    TypeAbs(String, Kind, Box<Type>),
    /// Type Type
    TypeApp(Box<Type>, Box<Type>),
    /// ∀ X::Kind. Type
    TypeAll(String, Kind, Box<Type>),
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
                let (new_context, name) = pick_fresh_name(context, tyX.clone());

                format!("λ{}::{}. {}", name, knK1, tyT2.to_string_type(&new_context))
            }
            Type::TypeAll(tyX, knK1, tyT2) => {
                let (new_context, name) = pick_fresh_name(context, tyX.clone());

                format!("∀{}::{}. {}", name, knK1, tyT2.to_string_type(&new_context))
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
            Type::TypeVar(x, n) => {
                if context.len() == *n as usize {
                    format!("{}", index_to_name(context, *x as usize))
                } else {
                    "OutOfBounds".to_string()
                }
            }
            t => format!("({})", t.to_string_type(context))
        }
    }
}

#[derive(Clone)]
pub enum Term {
    /// x, y, z ...
    TermVar(i64, i64),
    /// lambda x:Type. Term
    TermAbs(String, Type, Box<Term>),
    /// Term Term
    TermApp(Box<Term>, Box<Term>),
    /// lambda X::Kind. Term
    TermTypeAbs(String, Kind, Box<Term>),
    /// Term \[Type\]
    TermTypeApp(Box<Term>, Type),
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_term(&Context::new()))
    }
}

impl Term {
    fn to_string_atomic(&self, context: &Context) -> String {
        match self {
            Term::TermVar(i, n) => {
                if context.len() == *n as usize {
                    format!("{}", index_to_name(context, *i as usize))
                } else {
                    "OutOfBounds".to_string()
                }
            },
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
    NameBinding,
    VarBinding(Type),
    TyVarBinding(Kind)
}

pub type Context = VecDeque<(String, Binding)>;