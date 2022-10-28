use std::collections::HashMap;
use std::fmt::{Display, format, Formatter};
use std::sync::atomic::{AtomicU32, Ordering};

use crate::{Context, get_color, TypeVar};
use crate::base_type::BaseType;
use crate::constraint::Constraint;
use crate::kind::Kind;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /// X, Y, Z
    TypeVar(String),
    /// Type -> Type
    TypeArrow(Box<Type>, Box<Type>),
    /// λX::Kind. Type
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
    /// Eq a, Ord b => Type
    Qualified(Vec<Constraint>, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_type(&Context::new(), 0))
    }
}

impl Type {
    pub fn qualified(constraints: impl Into<Vec<Constraint>>, ty: impl Into<Type>) -> Type {
        Type::Qualified(constraints.into(), Box::new(ty.into()))
    }

    pub fn arrow(t1: impl Into<Type>, t2: impl Into<Type>) -> Type {
        Type::TypeArrow(Box::new(t1.into()), Box::new(t2.into()))
    }

    pub fn tuple3(t1: impl Into<Type>, t2: impl Into<Type>, t3: impl Into<Type>) -> Type {
        Type::Tuple(vec![
            t1.into(),
            t2.into(),
            t3.into(),
        ])
    }

    pub fn pair(t1: impl Into<Type>, t2: impl Into<Type>) -> Type {
        Type::Tuple(vec![
            t1.into(),
            t2.into(),
        ])
    }

    pub fn reference(t1: impl Into<Type>) -> Type {
        Type::Reference(Box::new(t1.into()))
    }

    pub fn type_var(&self) -> String {
        match self {
            Type::TypeVar(x) => x.clone(),
            _ => panic!("Not a type var")
        }
    }

    pub fn to_string_type(&self, context: &Context, color: u32) -> String {
        match self {
            Type::TypeAbs(tyX, knK1, tyT2) => {
                // Todo: Fix will not work with pick fresh name. We already require unique names for everything.
                let (new_context, name) = context.pick_fresh_name(tyX);

                format!("λ{}::{}. {}", name, knK1, tyT2.to_string_type(&new_context, color))
            }
            Type::TypeAll(tyX, knK1, tyT2) => {
                let (new_context, name) = context.pick_fresh_name(tyX);

                format!("∀{}::{}. {}", name, knK1, tyT2.to_string_type(&new_context, color))
            }
            Type::Qualified(constraints, rest) => {
                //let (new_context, name) = pick_fresh_name(context, tyX.clone());

                let formatted = constraints.iter().map(|constraint| {
                    let vars = constraint.vars.iter().map(|a| {
                        a.to_string_type(context, color)
                    }).collect::<Vec<_>>().join(" ");

                    format!("{} {}", constraint.ident, vars)
                }).collect::<Vec<_>>().join(", ");

                format!("{} => {}", formatted, rest.to_string_type(&context, color))
            }
            Type::Recursive(tyX, knK1, tyT2) => {
                let (new_context, name) = context.pick_fresh_name(tyX);

                format!("μ{}::{}. {}", name, knK1, tyT2.to_string_type(&new_context, color))
            }
            Type::Existential(x, knK1, tyT2) => {
                let (new_context, name) = context.pick_fresh_name(x);

                format!("{}∃{}::{}, {}{}", get_color(color, "{"), name, knK1, tyT2.to_string_type(&new_context, color + 1), get_color(color, "}"))
            }
            t => t.to_string_arrow(context, color),
        }
    }

    fn to_string_arrow(&self, context: &Context, color: u32) -> String {
        match self {
            Type::TypeArrow(tyT1, tyT2) => {
                format!("{} -> {}", tyT1.to_string_app_type(context, color), tyT2.to_string_arrow(context, color))
            }
            t => t.to_string_app_type(context, color),
        }
    }

    fn to_string_app_type(&self, context: &Context, color: u32) -> String {
        match self {
            Type::TypeApp(tyT1, tyT2) => {
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
            }
            Type::Variants(terms) => {
                let mut r = terms.iter().map(|(name, term)| {
                    let mut s = name.clone();
                    s.push_str(" = ");
                    s.push_str(&term.to_string_type(context, color + 1));
                    s
                }).collect::<Vec<_>>().join(", ");
                format!("{}{}{}", get_color(color, "<"), r, get_color(color, ">"))
            }
            Type::Base(b) => format!("{:?}", b),
            Type::Reference(b) => format!("&{}", b.to_string_type(context, color)),
            t => format!("{}{}{}", get_color(color, "("), t.to_string_type(context, color + 1), get_color(color, ")"))
        }
    }

    pub fn new_unique_var() -> Type {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        TypeVar(format!("#NEW{}", COUNTER.fetch_add(1, Ordering::Relaxed)))
    }
}

impl From<&str> for Type {
    fn from(s: &str) -> Self {
        Type::TypeVar(s.to_string())
    }
}

impl From<BaseType> for Type {
    fn from(s: BaseType) -> Self {
        Type::Base(s)
    }
}
