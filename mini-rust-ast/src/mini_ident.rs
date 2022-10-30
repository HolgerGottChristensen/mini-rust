use std::fmt::{Debug, Formatter};

use paris::formatter::colorize_string;
use proc_macro2::{Ident, Span};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::Token;
use syn::token::Token;
use mini_ir::Kind;
use mini_ir::Kind::{KindArrow, KindStar};

use crate::{IDENT_COLOR, ToMiniIrKind, ToMiniIrType};

#[derive(PartialEq, Clone)]
pub struct MiniIdent(pub Ident);

#[derive(PartialEq, Clone, Debug)]
pub enum MiniRecIdent {
    Ident(MiniIdent),
    RecIdent {
        ident: MiniIdent,
        lt_token: Token![<],
        inner: Punctuated<MiniRecIdent, Token![,]>,
        gt_token: Token![>]
    }
}

impl ToString for MiniIdent {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl Parse for MiniIdent {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(MiniIdent(Ident::parse(input)?))
    }
}

impl MiniIdent {
    pub fn new(s: &str) -> MiniIdent {
        MiniIdent(Ident::new(s, Span::call_site()))
    }
}

impl MiniRecIdent {
    pub fn ident(s: &str) -> MiniRecIdent {
        MiniRecIdent::Ident(
            MiniIdent(
                Ident::new(s, Span::call_site())
            )
        )
    }

    pub fn rec(s: &str, vec: Vec<MiniRecIdent>) -> MiniRecIdent {
        MiniRecIdent::RecIdent {
            ident: MiniIdent::new(s),
            lt_token: Default::default(),
            inner: Punctuated::from_iter(vec.into_iter()),
            gt_token: Default::default()
        }
    }
}

impl ToMiniIrKind for MiniRecIdent {
    fn convert_kind(&self) -> Kind {
        match self {
            MiniRecIdent::Ident(_) => {
                KindStar
            }
            MiniRecIdent::RecIdent { inner, .. } => {
                let mut res = KindStar;
                for ident in inner.iter().rev() {
                    res = KindArrow(Box::new(ident.convert_kind()), Box::new(res));
                }
                res
            }
        }
    }
}

impl ToString for MiniRecIdent {
    fn to_string(&self) -> String {
        match self {
            MiniRecIdent::Ident(ident) => {ident.to_string()}
            MiniRecIdent::RecIdent {ident, ..} => {ident.to_string()}
        }
    }
}

impl Parse for MiniRecIdent {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = MiniIdent::parse(input)?;
        if !input.peek(Token![<]) {
            return Ok(MiniRecIdent::Ident(ident));
        }
        let lt_token: Token![<] = input.parse()?;

        let mut params = Punctuated::new();
        loop {
            if input.peek(Token![>]) {
                break;
            }

            let inner = MiniRecIdent::parse(input)?;
            params.push_value(inner);

            if input.peek(Token![>]) {
                break;
            }
            let punct = input.parse()?;
            params.push_punct(punct);
        }

        let gt_token: Token![>] = input.parse()?;

        Ok(MiniRecIdent::RecIdent {
            ident,
            lt_token,
            inner: params,
            gt_token
        })
    }
}

impl Debug for MiniIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = colorize_string(format!("{}{}</>", IDENT_COLOR, self.0.to_string()));
        write!(f, "{}", string)
    }
}

mod test {
    use proc_macro2::{Ident, Span};
    use syn::{parse_quote, Token};
    use syn::parse::Parser;
    use syn::punctuated::Punctuated;
    use syn::token::Comma;
    use crate::{MiniIdent, MiniRecIdent};

    #[test]
    pub fn parse_simple_ident_success() {
        // Arrange

        // Act
        let ident: MiniRecIdent = parse_quote!(
            T
        );

        // Assert
        assert_eq!(ident, MiniRecIdent::Ident(MiniIdent::new("T")))
    }

    #[test]
    pub fn parse_with_single_inner_generic() {
        // Arrange

        // Act
        let ident: MiniRecIdent = parse_quote!(
            T<U>
        );

        // Assert
        let expected =
            MiniRecIdent::rec("T", vec![
                MiniRecIdent::ident("U")
            ]);

        assert_eq!(ident, expected)
    }

    #[test]
    pub fn parse_with_comma_seperated_inner_generic() {
        // Arrange

        // Act
        let ident: MiniRecIdent = parse_quote!(
            T<U, I>
        );

        // Assert
        let expected =
            MiniRecIdent::rec("T", vec![
                MiniRecIdent::ident("U"),
                MiniRecIdent::ident("I")
            ]);

        assert_eq!(ident, expected)
    }

    #[test]
    pub fn parse_with_nested_inner_generic() {
        // Arrange

        // Act
        let ident: MiniRecIdent = parse_quote!(
            T<U<I>>
        );

        // Assert
        let expected =
            MiniRecIdent::rec("T", vec![
                MiniRecIdent::rec("U", vec![
                    MiniRecIdent::ident("I")
                ])
            ]);

        assert_eq!(ident, expected)
    }

    #[test]
    pub fn parse_with_nested_inner_generic_complex() {
        // Arrange

        // Act
        let ident: MiniRecIdent = parse_quote!(
            T<U<F>, I<F>>
        );

        // Assert
        let expected =
            MiniRecIdent::rec("T", vec![
                MiniRecIdent::rec("U", vec![
                    MiniRecIdent::ident("F")
                ]),
                MiniRecIdent::rec("I", vec![
                    MiniRecIdent::ident("F")
                ])
            ]);

        assert_eq!(ident, expected)
    }
}