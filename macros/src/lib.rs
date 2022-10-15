mod mini_expr;
mod expr;
mod mini_item;
mod item;
mod mini_file;
mod stmt;
mod mini_stmt;
mod mini_pat;
mod mini_ident;
mod mini_type;
mod mini_path;
mod mini_generics;
mod util;

use proc_macro::TokenStream;
use chalk_integration::interner::ChalkIr;
use system_f_omega::{Context, Term, Type, type_of};

use syn::{parse_macro_input, DeriveInput, Expr};
use quote::quote;
use crate::expr::*;
use crate::item::*;
use mini_item::*;
use mini_file::*;
use mini_stmt::*;
use mini_ident::*;
use mini_type::*;
use mini_generics::*;
use crate::util::Env;

pub(crate) const IDENT_COLOR: &'static str = "<magenta><i>";
pub(crate) const TYPE_COLOR: &'static str = "<blue><b>";
pub(crate) const PAT_COLOR: &'static str = "<cyan>";
pub(crate) const PATH_COLOR: &'static str = "<bright-yellow>";

/// https://doc.rust-lang.org/reference/procedural-macros.html#function-like-procedural-macros
#[proc_macro]
pub fn mini_rust_ast(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as MiniFile);


    panic!("\n{:#?}", &input);
    let tokens = quote! {};
    tokens.into()
}

#[proc_macro]
pub fn mini_rust_to_system_f_omega(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as MiniFile);


    panic!("\n{:#?}", &input);
    let tokens = quote! {};
    tokens.into()
}

pub(crate) trait ToSystemFOmegaTerm {
    fn convert_term(&self) -> Term;
}

pub(crate) trait ToSystemFOmegaType {
    fn convert_type(&self) -> system_f_omega::Type;
}

pub(crate) trait ToChalkRustIR {
    fn convert(&self, env: &Env) -> chalk_solve::rust_ir::AdtDatum<ChalkIr>;
}