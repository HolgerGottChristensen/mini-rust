use std::fmt::{Debug, Formatter};
use std::mem;

use paris::formatter::colorize_string;
use proc_macro2::Span;
use syn::{Lit, LitFloat, Member, parenthesized, Path, PathArguments, Token, token};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

use mini_ir::Term;

use crate::{expr_ret, expr_struct_helper, MiniExprAssign, MiniExprBinary, MiniExprBlock, MiniExprBox, MiniExprCall, MiniExprField, MiniExprMatch, MiniExprMethodCall, MiniExprParen, MiniExprPath, MiniExprReference, MiniExprReturn, MiniExprStruct, MiniExprTuple, MiniExprUnary, MiniExprWhile, MiniIdent, parse_expr_box, parse_expr_unary, ToMiniIrTerm};
use crate::expr::MiniLitExpr;

#[derive(PartialEq, Clone)]
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

#[derive(PartialEq, Clone)]
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

impl MiniBinOp {
    pub fn name(&self) -> String {
        match self {
            MiniBinOp::Add(_) => "add".to_string(),
            MiniBinOp::Sub(_) => "sub".to_string(),
            MiniBinOp::Mul(_) => "mul".to_string(),
            MiniBinOp::Div(_) => "div".to_string(),
            MiniBinOp::Rem(_) => "rem".to_string(),
            MiniBinOp::And(_) => "and".to_string(),
            MiniBinOp::Or(_) => "or".to_string(),
            MiniBinOp::BitXor(_) => "bit_xor".to_string(),
            MiniBinOp::BitAnd(_) => "bit_and".to_string(),
            MiniBinOp::BitOr(_) => "bit_or".to_string(),
            MiniBinOp::Shl(_) => "left_shift".to_string(),
            MiniBinOp::Shr(_) => "right_shift".to_string(),
            MiniBinOp::Eq(_) => "PartialEq::eq".to_string(),
            MiniBinOp::Lt(_) => "lt".to_string(),
            MiniBinOp::Le(_) => "le".to_string(),
            MiniBinOp::Ne(_) => "ne".to_string(),
            MiniBinOp::Ge(_) => "ge".to_string(),
            MiniBinOp::Gt(_) => "gt".to_string(),
        }
    }
}

impl Debug for MiniBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MiniBinOp::Add(_) => {
                let s = colorize_string(format!("<bright-green>+</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Sub(_) => {
                let s = colorize_string(format!("<bright-green>-</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Mul(_) => {
                let s = colorize_string(format!("<bright-green>*</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Div(_) => {
                let s = colorize_string(format!("<bright-green>/</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Rem(_) => {
                let s = colorize_string(format!("<bright-green>%</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::And(_) => {
                let s = colorize_string(format!("<bright-green>&&</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Or(_) => {
                let s = colorize_string(format!("<bright-green>||</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::BitXor(_) => {
                let s = colorize_string(format!("<bright-green>^</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::BitAnd(_) => {
                let s = colorize_string(format!("<bright-green>&</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::BitOr(_) => {
                let s = colorize_string(format!("<bright-green>|</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Shl(_) => {
                let s = colorize_string(format!("<bright-green><<</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Shr(_) => {
                let s = colorize_string(format!("<bright-green>>></>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Eq(_) => {
                let s = colorize_string(format!("<bright-green>==</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Lt(_) => {
                let s = colorize_string(format!("<bright-green><=</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Le(_) => {
                let s = colorize_string(format!("<bright-green><</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Ne(_) => {
                let s = colorize_string(format!("<bright-green>!=</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Ge(_) => {
                let s = colorize_string(format!("<bright-green>>=</>"));
                write!(f, "{}", s)
            }
            MiniBinOp::Gt(_) => {
                let s = colorize_string(format!("<bright-green>></>"));
                write!(f, "{}", s)
            }
        }
    }
}

impl Debug for MiniUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MiniUnOp::Deref(_) => {
                let s = colorize_string(format!("<bright-green>*</>"));
                write!(f, "{}", s)
            }
            MiniUnOp::Not(_) => {
                let s = colorize_string(format!("<bright-green>!</>"));
                write!(f, "{}", s)
            }
            MiniUnOp::Neg(_) => {
                let s = colorize_string(format!("<bright-green>-</>"));
                write!(f, "{}", s)
            }
        }
    }
}

impl Debug for MiniExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MiniExpr::Assign(s) => s.fmt(f),
            MiniExpr::Binary(s) => s.fmt(f),
            MiniExpr::Block(s) => s.fmt(f),
            MiniExpr::Box(s) => s.fmt(f),
            MiniExpr::Call(s) => s.fmt(f),
            MiniExpr::Field(s) => s.fmt(f),
            MiniExpr::Lit(s) => s.fmt(f),
            MiniExpr::Match(s) => s.fmt(f),
            MiniExpr::MethodCall(s) => s.fmt(f),
            MiniExpr::Paren(s) => s.fmt(f),
            MiniExpr::Path(s) => s.fmt(f),
            MiniExpr::Reference(s) => s.fmt(f),
            MiniExpr::Return(s) => s.fmt(f),
            MiniExpr::Struct(s) => s.fmt(f),
            MiniExpr::Tuple(s) => s.fmt(f),
            MiniExpr::Unary(s) => s.fmt(f),
            MiniExpr::While(s) => s.fmt(f),
        }
    }
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

#[derive(PartialEq, Clone)]
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
                            method: MiniIdent(method),
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

impl ToMiniIrTerm for MiniExpr {
    fn convert_term(&self) -> Term {
        match self {
            MiniExpr::Assign(l) => l.convert_term(),
            MiniExpr::Binary(l) => l.convert_term(),
            MiniExpr::Block(l) => l.convert_term(),
            MiniExpr::Box(_) => todo!(),
            MiniExpr::Call(l) => l.convert_term(),
            MiniExpr::Field(l) => l.convert_term(),
            MiniExpr::Lit(l) => l.convert_term(),
            MiniExpr::Match(l) => l.convert_term(),
            MiniExpr::MethodCall(l) => l.convert_term(),
            MiniExpr::Paren(l) => l.convert_term(),
            MiniExpr::Path(l) => l.convert_term(),
            MiniExpr::Reference(l) => l.convert_term(),
            MiniExpr::Return(l) => l.convert_term(),
            MiniExpr::Struct(l) => l.convert_term(),
            MiniExpr::Tuple(l) => l.convert_term(),
            MiniExpr::Unary(l) => l.convert_term(),
            MiniExpr::While(l) => l.convert_term(),
        }
    }
}

mod tests {
    use std::collections::HashMap;

    use syn::parse_quote;

    use mini_ir::{BaseType, Binding, Bool, Context, Int, kind_of, Substitutions, Term, Type, type_of, Unit};

    use crate::ToMiniIrTerm;
    use crate::mini_expr::MiniExpr;

    #[test]
    fn parse_int_reference() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            &0
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&Context::new(), converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_tuple_1_element() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            (1, )
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&Context::new(), converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_tuple_multiple() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            (1, 32.2)
        );

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&Context::new(), converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&Context::new(), converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_unary_neg() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            -3
        );

        let context = Context::new();
        let context = context.add_binding(Binding::VarBinding("neg".to_string(), Type::TypeArrow(
            Box::new(Type::Base(BaseType::Int)),
            Box::new(Type::Base(BaseType::Int)),
        )));

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_unary_not() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            !false
        );

        let context = Context::new();
        let context = context.add_binding(Binding::VarBinding("not".to_string(), Type::TypeArrow(
            Box::new(Type::Base(BaseType::Bool)),
            Box::new(Type::Base(BaseType::Bool)),
        )));

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_binary_add() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            2 + 2
        );

        let context = Context::new();
        let context = context.add_binding(Binding::VarBinding("add".to_string(), Type::arrow(Unit, Type::arrow(Int, Type::arrow(Int, Int)))));

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_binary_eq() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            1 == 2
        );

        let context = Context::new();
        let context = context.add_binding(Binding::VarBinding("PartialEq::eq".to_string(), Type::arrow(Unit, Type::arrow(Int, Type::arrow(Int, Bool)))));

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new());
        println!("Type: {}", &converted_type.as_ref().map(|r| r.to_string_type(&context, 0)).unwrap_or_else(|w| w.to_string()));

        /*let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);*/

        // Assert
        assert_eq!(converted_type, Ok(Type::Base(Bool)))
    }

    #[test]
    fn parse_block_simple() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                2
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_block_local_variable() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                let x = 0;
                x
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_block_multiple_local_variables() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                let x = 0;
                let y = true;
                let z = ();
                let w = 42.0;
                y
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_block_statements() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                1;
                2;
                true
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_block_mixed() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                let x = 1;
                2;
                let y = true;

                x
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_function_call_simple() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            function(3)
        );

        let context = Context::new();
        let context = context.add_binding(Binding::VarBinding("function".to_string(), Type::TypeArrow(
            Box::new(Type::Base(BaseType::Unit)),
            Box::new(Type::TypeArrow(
                Box::new(Type::Base(BaseType::Int)),
                Box::new(Type::Base(BaseType::Bool)),
            )),
        )));

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_function_call_multiple_arguments() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            function(3, true)
        );

        let context = Context::new();
        let context = context.add_binding(Binding::VarBinding("function".to_string(), Type::TypeArrow(
            Box::new(Type::Base(BaseType::Unit)),
            Box::new(Type::TypeArrow(
                Box::new(Type::Base(BaseType::Int)),
                Box::new(Type::TypeArrow(
                    Box::new(Type::Base(BaseType::Bool)),
                    Box::new(Type::Base(BaseType::Bool)),
                )),
            )),
        )));

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_tuple_projection() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            (42.0, 42, true).2
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        let converted_kind = kind_of(&context, converted_type.clone());

        println!("Lambda: {}", &converted);
        println!("Type: {}", &converted_type);
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_tuple_projection_multiple() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            (42.0, (42, 3.14), true).1.0
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_struct_projection() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            s.field2
        );

        let context = Context::new();
        let context = context.add_binding(Binding::VarBinding(
            "s".to_string(),
            Type::Record(HashMap::from([
                ("field1".to_string(), Type::Base(BaseType::Int)),
                ("field2".to_string(), Type::Base(BaseType::Float)),
            ])),
        ));

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }


    #[test]
    fn parse_block_local_struct() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                struct Test {}
                1
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_block_local_struct_with_fields() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                struct Test {
                    test: i64
                }

                1
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_block_local_struct_with_generics() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                struct Test<T> {
                    test: T
                }

                1
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);


        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_block_local_struct_with_fields_and_creation() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                struct Test {
                    test: i64
                }

                struct Test2 {
                    test: i64
                }

                let x = Test {test: 2};
                x.test
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }

    #[test]
    fn parse_block_local_struct_with_fields_and_creation_and_generics() {
        // Arrange
        let mini: MiniExpr = parse_quote!(
            {
                struct Test<T> {
                    test: T
                }

                let x = Test::<i64>{test: 2};
                x.test
            }
        );

        let context = Context::new();

        println!("\n{:#?}", &mini);

        // Act
        let converted = mini.convert_term();
        println!("Lambda: {}", &converted);

        let converted_type = type_of(&context, converted.clone(), &mut Substitutions::new()).unwrap();
        println!("Type: {}", &converted_type);

        let converted_kind = kind_of(&context, converted_type.clone());
        println!("Kind: {}", &converted_kind);

        // Assert
        //assert_eq!(converted, Term::Reference(Box::new(Term::Integer(0))))
    }
}