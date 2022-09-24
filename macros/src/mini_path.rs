use std::fmt::{Debug, Formatter};
use paris::formatter::colorize_string;
use quote::ToTokens;
use syn::Path;
use crate::PATH_COLOR;

pub struct MiniPath(pub Path);

impl Debug for MiniPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = colorize_string(format!("{}{}</>", PATH_COLOR, self.0.to_token_stream().to_string()));
        write!(f, "{}", string)
    }
}