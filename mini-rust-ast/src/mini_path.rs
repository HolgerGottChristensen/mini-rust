use std::fmt::{Debug, Formatter};
use paris::formatter::colorize_string;
use quote::ToTokens;
use syn::{GenericArgument, Path, PathArguments};
use crate::{MiniType, PATH_COLOR, ToSystemFOmegaType};
use mini_ir::Type as FType;

#[derive(PartialEq, Clone)]
pub struct MiniPath(pub Path);

impl Debug for MiniPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = colorize_string(format!("{}{}</>", PATH_COLOR, self.0.to_token_stream().to_string()));
        write!(f, "{}", string)
    }
}

impl MiniPath {
    pub fn as_ident(&self) -> String {
        assert!(self.0.leading_colon == None);
        let idents = self.0.segments.iter().map(|a| a.ident.to_string()).collect::<Vec<_>>();

        idents.join("::")
    }

    pub fn generics(&self) -> Vec<FType> {
        assert!(self.0.leading_colon == None);
        let generics = self.0.segments.iter().flat_map(|a| {
            match &a.arguments {
                PathArguments::None => vec![],
                PathArguments::AngleBracketed(s) => {
                    s.args.iter().map(|a| match a {
                        GenericArgument::Lifetime(_) => todo!(),
                        GenericArgument::Type(t) => MiniType(t.clone()).convert_type(),
                        GenericArgument::Const(_) => todo!(),
                        GenericArgument::Binding(_) => todo!(),
                        GenericArgument::Constraint(_) => todo!(),
                    }).collect()
                }
                PathArguments::Parenthesized(_) => panic!("Paths with generics can not contain paranthesis")
            }
        }).collect::<Vec<_>>();

        generics
    }
}