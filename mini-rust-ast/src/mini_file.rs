use syn::parse::{Parse, ParseStream};
use syn::token::RArrow;
use mini_ir::{BaseType, Term};

use crate::{MiniItem, ToMiniIrTerm, ToMiniIrType};
use crate::item::MiniFn;
use crate::mini_type::MiniType;
use crate::util::replace_inner;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniFile {
    pub items: Vec<MiniItem>,
}

impl Parse for MiniFile {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(MiniFile {
            items: {
                let mut items = Vec::new();
                while !input.is_empty() {
                    items.push(input.parse()?);
                }
                items
            },
        })
    }
}

impl ToMiniIrTerm for MiniFile {
    fn convert_term(&self) -> Term {
        fn is_main(item: &MiniItem) -> bool {
            match item {
                MiniItem::Fn(MiniFn{ ident, .. }) => ident.to_string() == "main".to_string(),
                _ => false,
            }
        }

        let mut body = if let Some(MiniItem::Fn(MiniFn { block, return_type, .. })) = self.items.iter().find(|i| is_main(*i)) {
            let ret = match return_type {
                None => mini_ir::Type::Base(BaseType::Unit),
                Some(r) => r.1.convert_type()
            };

            Term::Ascribe(Box::new(block.as_ref().unwrap().convert_term()), ret)
        } else {
            Term::Unit
        };

        for stmt in self.items.iter().rev().filter(|i| !is_main(*i)) {
            body = replace_inner(stmt.convert_term(), body);
        }

        body
    }
}