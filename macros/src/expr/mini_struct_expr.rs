use syn::{braced, FieldValue, Path, token, Token};
use syn::parse::ParseStream;
use syn::punctuated::Punctuated;
use crate::mini_expr::MiniExpr;

#[derive(PartialEq, Clone, Debug)]
pub struct MiniExprStruct {
    pub path: Path,
    pub brace_token: token::Brace,
    pub fields: Punctuated<FieldValue, Token![,]>,
    pub dot2_token: Option<Token![..]>,
    pub rest: Option<Box<MiniExpr>>,
}


pub fn expr_struct_helper(input: ParseStream, path: Path) -> syn::Result<MiniExprStruct> {
    let content;
    let brace_token = braced!(content in input);

    let mut fields = Punctuated::new();
    while !content.is_empty() {
        if content.peek(Token![..]) {
            return Ok(MiniExprStruct {
                brace_token,
                path,
                fields,
                dot2_token: Some(content.parse()?),
                rest: if content.is_empty() {
                    None
                } else {
                    Some(Box::new(content.parse()?))
                },
            });
        }

        fields.push(content.parse()?);
        if content.is_empty() {
            break;
        }
        let punct: Token![,] = content.parse()?;
        fields.push_punct(punct);
    }

    Ok(MiniExprStruct {
        brace_token,
        path,
        fields,
        dot2_token: None,
        rest: None,
    })
}