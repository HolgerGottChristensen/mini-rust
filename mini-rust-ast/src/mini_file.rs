use syn::parse::{Parse, ParseStream};

use crate::MiniItem;

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