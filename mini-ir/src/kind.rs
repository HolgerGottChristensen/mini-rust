use std::fmt::{Display, Formatter};

use paris::formatter::colorize_string;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
pub enum Kind {
    /// *
    KindStar,
    /// Kind -> Kind
    KindArrow(Box<Kind>, Box<Kind>),
}

impl Kind {
    fn to_string_kind(&self) -> String {
        match self {
            Kind::KindStar => self.to_string_atomic(),
            Kind::KindArrow(k1, k2) => {
                colorize_string(format!("{} <bright-blue><b>=></> {}", k1.to_string_atomic(), k2.to_string_kind()))
            }
        }
    }

    fn to_string_atomic(&self) -> String {
        match self {
            Kind::KindStar => colorize_string("<bright-blue><b>*</>"),
            x => format!("({})", x.to_string_kind()),
        }
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_kind())
    }
}
