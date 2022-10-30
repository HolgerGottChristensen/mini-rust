use proc_macro::TokenStream;

use quote::quote;
use syn::parse_macro_input;

use mini_file::*;
use mini_generics::*;
use mini_ident::*;
use mini_ir::Term;
use mini_item::*;
use mini_stmt::*;
use mini_type::*;

use crate::expr::*;
use crate::item::*;

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
pub fn mini_rust_to_mini_ir(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as MiniFile);


    panic!("\n{:#?}", &input);
    let tokens = quote! {};
    tokens.into()
}

pub(crate) trait ToMiniIrTerm {
    fn convert_term(&self) -> Term;
}

pub(crate) trait ToMiniIrType {
    fn convert_type(&self) -> mini_ir::Type;
}

pub(crate) trait ToMiniIrKind {
    fn convert_kind(&self) -> mini_ir::Kind;
}