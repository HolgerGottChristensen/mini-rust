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

extern crate proc_macro;
use proc_macro::TokenStream;

use syn::{parse_macro_input, DeriveInput};
use quote::quote;
use crate::expr::*;
use crate::item::*;
use mini_item::*;
use mini_file::*;
use mini_stmt::*;
use mini_ident::*;
use mini_type::*;

pub(crate) const IDENT_COLOR: &'static str = "<magenta><i>";
pub(crate) const TYPE_COLOR: &'static str = "<blue><b>";
pub(crate) const PAT_COLOR: &'static str = "<cyan>";
pub(crate) const PATH_COLOR: &'static str = "<bright-yellow>";

/// Example of [function-like procedural macro][1].
///
/// [1]: https://doc.rust-lang.org/reference/procedural-macros.html#function-like-procedural-macros
#[proc_macro]
pub fn my_macro(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as MiniFile);


    panic!("\n{:#?}", &input);
    let tokens = quote! {};
    tokens.into()
}

/// Example of user-defined [derive mode macro][1]
///
/// [1]: https://doc.rust-lang.org/reference/procedural-macros.html#derive-mode-macros
#[proc_macro_derive(MyDerive)]
pub fn my_derive(_input: TokenStream) -> TokenStream {
    let tokens = quote! {
        struct Hello;
    };

    tokens.into()
}

/// Example of user-defined [procedural macro attribute][1].
///
/// [1]: https://doc.rust-lang.org/reference/procedural-macros.html#attribute-macros
#[proc_macro_attribute]
pub fn my_attribute(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let tokens = quote! {
        #input

        struct Hello;
    };

    tokens.into()
}
