use std::mem;
use proc_macro2::{Ident, Span};
use syn::{BinOp, Error, Expr, Lit, LitFloat, Member, parenthesized, Path, PathArguments, Token, token};
use syn::parse::{Parse, ParseBuffer, ParseStream};
use syn::punctuated::Punctuated;
use crate::expr::MiniLitExpr;
use crate::{expr_ret, expr_struct_helper, MiniExprAssign, MiniExprBinary, MiniExprBlock, MiniExprBox, MiniExprCall, MiniExprField, MiniExprMatch, MiniExprMethodCall, MiniExprParen, MiniExprPath, MiniExprReference, MiniExprReturn, MiniExprStruct, MiniExprTuple, MiniExprUnary, MiniExprWhile, parse_expr_box, parse_expr_unary};

#[derive(PartialEq, Clone, Debug)]
pub enum MiniExpr {
    Assign(MiniExprAssign),
    Binary(MiniExprBinary),
    Block(MiniExprBlock),
    Box(MiniExprBox),
    Call(MiniExprCall),
    Field(MiniExprField),
    Lit(MiniLitExpr),
    Match(MiniExprMatch),
    MethodCall(MiniExprMethodCall),
    Paren(MiniExprParen),
    Path(MiniExprPath),
    Reference(MiniExprReference),
    Return(MiniExprReturn),
    Struct(MiniExprStruct),
    Tuple(MiniExprTuple),
    Unary(MiniExprUnary),
    While(MiniExprWhile),
}

#[derive(Clone, Copy)]
pub struct AllowStruct(bool);

#[derive(PartialEq, Clone, Debug, Ord, PartialOrd, Eq)]
enum MiniPrecedence {
    Any,
    Assign,
    Range,
    Or,
    And,
    Compare,
    BitOr,
    BitXor,
    BitAnd,
    Shift,
    Arithmetic,
    Term,
    Cast,
}

impl MiniPrecedence {
    fn of(op: &MiniBinOp) -> Self {
        match *op {
            MiniBinOp::Add(_) | MiniBinOp::Sub(_) => MiniPrecedence::Arithmetic,
            MiniBinOp::Mul(_) | MiniBinOp::Div(_) | MiniBinOp::Rem(_) => MiniPrecedence::Term,
            MiniBinOp::And(_) => MiniPrecedence::And,
            MiniBinOp::Or(_) => MiniPrecedence::Or,
            MiniBinOp::BitXor(_) => MiniPrecedence::BitXor,
            MiniBinOp::BitAnd(_) => MiniPrecedence::BitAnd,
            MiniBinOp::BitOr(_) => MiniPrecedence::BitOr,
            MiniBinOp::Shl(_) | MiniBinOp::Shr(_) => MiniPrecedence::Shift,
            MiniBinOp::Eq(_)
            | MiniBinOp::Lt(_)
            | MiniBinOp::Le(_)
            | MiniBinOp::Ne(_)
            | MiniBinOp::Ge(_)
            | MiniBinOp::Gt(_) => MiniPrecedence::Compare,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum MiniBinOp {
    /// The `+` operator (addition)
    Add(Token![+]),
    /// The `-` operator (subtraction)
    Sub(Token![-]),
    /// The `*` operator (multiplication)
    Mul(Token![*]),
    /// The `/` operator (division)
    Div(Token![/]),
    /// The `%` operator (modulus)
    Rem(Token![%]),
    /// The `&&` operator (logical and)
    And(Token![&&]),
    /// The `||` operator (logical or)
    Or(Token![||]),
    /// The `^` operator (bitwise xor)
    BitXor(Token![^]),
    /// The `&` operator (bitwise and)
    BitAnd(Token![&]),
    /// The `|` operator (bitwise or)
    BitOr(Token![|]),
    /// The `<<` operator (shift left)
    Shl(Token![<<]),
    /// The `>>` operator (shift right)
    Shr(Token![>>]),
    /// The `==` operator (equality)
    Eq(Token![==]),
    /// The `<` operator (less than)
    Lt(Token![<]),
    /// The `<=` operator (less than or equal to)
    Le(Token![<=]),
    /// The `!=` operator (not equal to)
    Ne(Token![!=]),
    /// The `>=` operator (greater than or equal to)
    Ge(Token![>=]),
    /// The `>` operator (greater than)
    Gt(Token![>]),
}

impl Parse for MiniBinOp {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_binop(input)
    }
}

fn parse_binop(input: ParseStream) -> syn::Result<MiniBinOp> {
    if input.peek(Token![&&]) {
        input.parse().map(MiniBinOp::And)
    } else if input.peek(Token![||]) {
        input.parse().map(MiniBinOp::Or)
    } else if input.peek(Token![<<]) {
        input.parse().map(MiniBinOp::Shl)
    } else if input.peek(Token![>>]) {
        input.parse().map(MiniBinOp::Shr)
    } else if input.peek(Token![==]) {
        input.parse().map(MiniBinOp::Eq)
    } else if input.peek(Token![<=]) {
        input.parse().map(MiniBinOp::Le)
    } else if input.peek(Token![!=]) {
        input.parse().map(MiniBinOp::Ne)
    } else if input.peek(Token![>=]) {
        input.parse().map(MiniBinOp::Ge)
    } else if input.peek(Token![+]) {
        input.parse().map(MiniBinOp::Add)
    } else if input.peek(Token![-]) {
        input.parse().map(MiniBinOp::Sub)
    } else if input.peek(Token![*]) {
        input.parse().map(MiniBinOp::Mul)
    } else if input.peek(Token![/]) {
        input.parse().map(MiniBinOp::Div)
    } else if input.peek(Token![%]) {
        input.parse().map(MiniBinOp::Rem)
    } else if input.peek(Token![^]) {
        input.parse().map(MiniBinOp::BitXor)
    } else if input.peek(Token![&]) {
        input.parse().map(MiniBinOp::BitAnd)
    } else if input.peek(Token![|]) {
        input.parse().map(MiniBinOp::BitOr)
    } else if input.peek(Token![<]) {
        input.parse().map(MiniBinOp::Lt)
    } else if input.peek(Token![>]) {
        input.parse().map(MiniBinOp::Gt)
    } else {
        Err(input.error("expected binary operator"))
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum MiniUnOp {
    /// The `*` operator for dereferencing
    Deref(Token![*]),
    /// The `!` operator for logical inversion
    Not(Token![!]),
    /// The `-` operator for negation
    Neg(Token![-]),
}

impl Parse for MiniUnOp {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![*]) {
            input.parse().map(MiniUnOp::Deref)
        } else if lookahead.peek(Token![!]) {
            input.parse().map(MiniUnOp::Not)
        } else if lookahead.peek(Token![-]) {
            input.parse().map(MiniUnOp::Neg)
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for MiniExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        MiniExpr::parse_ambiguous_expr(input, AllowStruct(true))
    }
}

impl MiniExpr {
    pub fn parse_expr_early(input: ParseStream) -> syn::Result<MiniExpr> {
        let mut expr = if input.peek(Token![while]) {
            MiniExpr::While(input.parse()?)
        } else if input.peek(Token![match]) {
            MiniExpr::Match(input.parse()?)
        } else if input.peek(token::Brace) {
            MiniExpr::Block(input.parse()?)
        } else {
            let allow_struct = AllowStruct(true);
            let mut expr = MiniExpr::parse_unary_expr(input, allow_struct)?;

            return MiniExpr::parse_expr(input, expr, allow_struct, MiniPrecedence::Any);
        };

        if input.peek(Token![.]) && !input.peek(Token![..]) {
            expr = MiniExpr::trailer_helper(input, expr)?;
            let allow_struct = AllowStruct(true);

            return MiniExpr::parse_expr(input, expr, allow_struct, MiniPrecedence::Any);
        }

        Ok(expr)
    }

    fn parse_expr(
        input: ParseStream,
        mut lhs: MiniExpr,
        allow_struct: AllowStruct,
        base: MiniPrecedence,
    ) -> syn::Result<MiniExpr> {
        loop {
            if input
                .fork()
                .parse::<MiniBinOp>()
                .ok()
                .map_or(false, |op| MiniPrecedence::of(&op) >= base)
            {
                let op: MiniBinOp = input.parse()?;
                let precedence = MiniPrecedence::of(&op);
                let mut rhs = MiniExpr::parse_unary_expr(input, allow_struct)?;

                loop {
                    let next = MiniExpr::peek_precedence(input);
                    if next > precedence || next == precedence && precedence == MiniPrecedence::Assign {
                        rhs = MiniExpr::parse_expr(input, rhs, allow_struct, next)?;
                    } else {
                        break;
                    }
                }

                lhs = MiniExpr::Binary(MiniExprBinary {
                    left: Box::new(lhs),
                    op,
                    right: Box::new(rhs),
                });

            } else if MiniPrecedence::Assign >= base
                && input.peek(Token![=])
                && !input.peek(Token![==])
                && !input.peek(Token![=>])
            {
                let eq_token: Token![=] = input.parse()?;
                let mut rhs = MiniExpr::parse_unary_expr(input, allow_struct)?;
                loop {
                    let next = MiniExpr::peek_precedence(input);
                    if next >= MiniPrecedence::Assign {
                        rhs = MiniExpr::parse_expr(input, rhs, allow_struct, next)?;
                    } else {
                        break;
                    }
                }

                lhs = MiniExpr::Assign(MiniExprAssign {
                    left: Box::new(lhs),
                    eq_token,
                    right: Box::new(rhs),
                });

            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn peek_precedence(input: ParseStream) -> MiniPrecedence {
        if let Ok(op) = input.fork().parse() {
            MiniPrecedence::of(&op)
        } else if input.peek(Token![=]) && !input.peek(Token![=>]) {
            MiniPrecedence::Assign
        } else if input.peek(Token![..]) {
            MiniPrecedence::Range
        } else if input.peek(Token![as])
            || input.peek(Token![:]) && !input.peek(Token![::])
        {
            MiniPrecedence::Cast
        } else {
            MiniPrecedence::Any
        }
    }

    pub fn parse_ambiguous_expr(input: ParseStream, allow_struct: AllowStruct) -> syn::Result<MiniExpr> {
        let lhs = MiniExpr::parse_unary_expr(input, allow_struct)?;
        MiniExpr::parse_expr(input, lhs, allow_struct, MiniPrecedence::Any)
    }

    /// <UnOp> <trailer>
    /// & <trailer>
    /// &mut <trailer>
    /// box <trailer>
    pub(crate) fn parse_unary_expr(input: ParseStream, allow_struct: AllowStruct) -> syn::Result<MiniExpr> {
        let begin = input.fork();

        if input.peek(Token![&]) {
            let and_token: Token![&] = input.parse()?;
            let mutability: Option<Token![mut]> = input.parse()?;
            let expr = Box::new(MiniExpr::parse_unary_expr(input, allow_struct)?);

            Ok(MiniExpr::Reference(MiniExprReference {
                and_token,
                mutability,
                expr,
            }))

        } else if input.peek(Token![box]) {
            parse_expr_box(input, allow_struct).map(MiniExpr::Box)
        } else if input.peek(Token![*]) || input.peek(Token![!]) || input.peek(Token![-]) {
            parse_expr_unary(input, allow_struct).map(MiniExpr::Unary)
        } else {
            MiniExpr::trailer_expr(input, allow_struct)
        }
    }

    // <atom> (..<args>) ...
    // <atom> . <ident> (..<args>) ...
    // <atom> . <ident> ...
    // <atom> . <lit> ...
    // <atom> [ <expr> ] ...
    // <atom> ? ...
    fn trailer_expr(
        input: ParseStream,
        allow_struct: AllowStruct,
    ) -> syn::Result<MiniExpr> {
        let mut e = MiniExpr::atom_expr(input, allow_struct)?;

        MiniExpr::trailer_helper(input, e)
    }

    fn trailer_helper(input: ParseStream, mut e: MiniExpr) -> syn::Result<MiniExpr> {
        loop {
            if input.peek(token::Paren) {
                let content;

                e = MiniExpr::Call(MiniExprCall {
                    func: Box::new(e),
                    paren_token: parenthesized!(content in input),
                    args: content.parse_terminated(MiniExpr::parse)?,
                });
            } else if input.peek(Token![.])
                && !input.peek(Token![..])
            {
                let mut dot_token: Token![.] = input.parse()?;

                let float_token: Option<LitFloat> = input.parse()?;
                if let Some(float_token) = float_token {
                    if MiniExpr::multi_index(&mut e, &mut dot_token, float_token)? {
                        continue;
                    }
                }

                let member: Member = input.parse()?;

                if input.peek(token::Paren) {
                    if let Member::Named(method) = member {
                        let content;
                        e = MiniExpr::MethodCall(MiniExprMethodCall {
                            receiver: Box::new(e),
                            dot_token,
                            method,
                            paren_token: parenthesized!(content in input),
                            args: content.parse_terminated(MiniExpr::parse)?,
                        });
                        continue;
                    }
                }

                e = MiniExpr::Field(MiniExprField {
                    base: Box::new(e),
                    dot_token,
                    member,
                });
            } else {
                break;
            }
        }
        Ok(e)
    }

    // Parse all atomic expressions which don't have to worry about precedence
    // interactions, as they are fully contained.
    fn atom_expr(input: ParseStream, allow_struct: AllowStruct) -> syn::Result<MiniExpr> {
        if input.peek(Lit) {
            input.parse().map(MiniExpr::Lit)
        } else if input.peek(syn::Ident)
            || input.peek(Token![::])
            || input.peek(Token![<])
            || input.peek(Token![self])
            || input.peek(Token![Self])
            || input.peek(Token![super])
            || input.peek(Token![crate])
        {
            MiniExpr::path_or_macro_or_struct(input, allow_struct)
        } else if input.peek(token::Paren) {
            MiniExpr::paren_or_tuple(input)
        } else if input.peek(Token![return]) {
            expr_ret(input, allow_struct).map(MiniExpr::Return)
        } else if input.peek(Token![while]) {
            input.parse().map(MiniExpr::While)
        } else if input.peek(Token![match]) {
            input.parse().map(MiniExpr::Match)
        } else if input.peek(token::Brace) {
            input.parse().map(MiniExpr::Block)
        } else {
            Err(input.error("expected expression"))
        }
    }

    pub fn requires_terminator(&self) -> bool {
        // see https://github.com/rust-lang/rust/blob/2679c38fc/src/librustc_ast/util/classify.rs#L7-L25
        match *self {
            MiniExpr::Block(..)
            | MiniExpr::Match(..)
            | MiniExpr::While(..) => false,
            _ => true,
        }
    }

    fn path_or_macro_or_struct(input: ParseStream, allow_struct: AllowStruct) -> syn::Result<MiniExpr> {
        let expr: MiniExprPath = input.parse()?;

        if expr.qself.is_none() && input.peek(Token![!]) && !input.peek(Token![!=]) {
            let mut contains_arguments = false;
            for segment in &expr.path.segments {
                match segment.arguments {
                    PathArguments::None => {}
                    PathArguments::AngleBracketed(_) | PathArguments::Parenthesized(_) => {
                        contains_arguments = true;
                    }
                }
            }
        }

        if allow_struct.0 && input.peek(token::Brace) {
            let expr_struct = expr_struct_helper(input, expr.path)?;
            if expr.qself.is_some() {
                //Ok(Expr::Verbatim(verbatim::between(begin, input)))
                Err(syn::Error::new(Span::call_site(), "qself is not valid here"))
            } else {
                Ok(MiniExpr::Struct(expr_struct))
            }
        } else {
            Ok(MiniExpr::Path(expr))
        }
    }

    fn paren_or_tuple(input: ParseStream) -> syn::Result<MiniExpr> {
        let content;
        let paren_token = parenthesized!(content in input);
        if content.is_empty() {
            return Ok(MiniExpr::Tuple(MiniExprTuple {
                paren_token,
                elems: Punctuated::new(),
            }));
        }

        let first: MiniExpr = content.parse()?;
        if content.is_empty() {
            return Ok(MiniExpr::Paren(MiniExprParen {
                paren_token,
                expr: Box::new(first),
            }));
        }

        let mut elems = Punctuated::new();
        elems.push_value(first);
        while !content.is_empty() {
            let punct = content.parse()?;
            elems.push_punct(punct);
            if content.is_empty() {
                break;
            }
            let value = content.parse()?;
            elems.push_value(value);
        }
        Ok(MiniExpr::Tuple(MiniExprTuple {
            paren_token,
            elems,
        }))
    }

    pub fn multi_index(e: &mut MiniExpr, dot_token: &mut Token![.], float: LitFloat) -> syn::Result<bool> {
        let mut float_repr = float.to_string();
        let trailing_dot = float_repr.ends_with('.');
        if trailing_dot {
            float_repr.truncate(float_repr.len() - 1);
        }
        for part in float_repr.split('.') {
            let index = syn::parse_str(part).map_err(|err| syn::Error::new(float.span(), err))?;

            let base = mem::replace(e, MiniExpr::DUMMY);

            *e = MiniExpr::Field(MiniExprField {
                base: Box::new(base),
                dot_token: Token![.](dot_token.span),
                member: Member::Unnamed(index),
            });
            *dot_token = Token![.](float.span());
        }
        Ok(!trailing_dot)
    }

    pub fn parse_without_eager_brace(input: ParseStream) -> syn::Result<MiniExpr> {
        MiniExpr::parse_ambiguous_expr(input, AllowStruct(false))
    }

    const DUMMY: Self = MiniExpr::Path(MiniExprPath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: Punctuated::new(),
        },
    });
}
