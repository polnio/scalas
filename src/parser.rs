use crate::error::Error;
use crate::tokenizer::Token;
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use derive_more::Deref;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Deref)]
pub struct Ident<'src> {
    #[deref]
    pub inner: &'src str,
    pub span: SimpleSpan,
}
impl<'src> Display for Ident<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deref)]
pub struct Literal<'src> {
    #[deref]
    pub inner: crate::tokenizer::Literal<'src>,
    pub span: SimpleSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnCall<'src> {
    pub ident: Ident<'src>,
    pub args: Vec<Expr<'src>>,
    pub span: SimpleSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block<'src> {
    pub stmts: Vec<Stmt<'src>>,
    pub span: SimpleSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<'src> {
    Literal(Literal<'src>),
    FnCall(FnCall<'src>),
    Ident(Ident<'src>),
    Block(Block<'src>),
}
impl Expr<'_> {
    pub fn span(&self) -> SimpleSpan {
        match self {
            Expr::Literal(l) => l.span,
            Expr::FnCall(f) => f.span,
            Expr::Ident(i) => i.span,
            Expr::Block(b) => b.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type<'src> {
    pub ident: Ident<'src>,
    pub generics: Vec<Type<'src>>,
    pub span: SimpleSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assign<'src> {
    pub ident: Ident<'src>,
    pub expr: Expr<'src>,
    pub span: SimpleSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt<'src> {
    Expr(Expr<'src>),
    Assign(Assign<'src>),
}
impl Stmt<'_> {
    pub fn span(&self) -> SimpleSpan {
        match self {
            Stmt::Expr(e) => e.span(),
            Stmt::Assign(a) => a.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fn<'src> {
    pub ident: Ident<'src>,
    pub args: Vec<(Ident<'src>, Type<'src>)>,
    pub ret: Type<'src>,
    pub body: Expr<'src>,
    pub span: SimpleSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Object<'src> {
    pub ident: Ident<'src>,
    pub fns: Vec<Fn<'src>>,
    pub span: SimpleSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<'src> {
    pub objects: Vec<Object<'src>>,
    pub span: SimpleSpan,
}

pub fn parser<'src, I>() -> impl Parser<'src, I, Program<'src>, extra::Err<Error<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let ident = select!(Token::Identifier(i) => i).map_with(|i, e| Ident {
        inner: i,
        span: e.span(),
    });
    let expr = recursive(|expr| {
        let literal = select!(Token::Literal(l) => l).map_with(|l, e| Literal {
            inner: l,
            span: e.span(),
        });
        let fn_call = group((
            ident,
            expr.clone()
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        ))
        .map_with(|(ident, args), e| FnCall {
            ident,
            args,
            span: e.span(),
        });

        let assign = just(Token::Val)
            .ignore_then(ident)
            .then_ignore(just(Token::Equals))
            .then(expr.clone())
            .map_with(|(ident, expr), e| Assign {
                ident,
                expr,
                span: e.span(),
            });

        let stmt = choice((assign.map(Stmt::Assign), expr.map(Stmt::Expr)));
        let block = stmt
            .repeated()
            .collect()
            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
            .map_with(|stmts, e| Block {
                stmts,
                span: e.span(),
            });

        let expr = choice((
            fn_call.map(Expr::FnCall),
            ident.map(Expr::Ident),
            literal.map(Expr::Literal),
            block.map(Expr::Block),
        ));
        expr
    });

    let type_ = recursive(|type_| {
        group((
            ident,
            type_
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
                .or_not(),
        ))
        .map_with(|(ident, generics), e| Type {
            ident,
            generics: generics.unwrap_or_default(),
            span: e.span(),
        })
    });

    let fn_ = group((
        just(Token::Def).ignore_then(ident),
        ident
            .then_ignore(just(Token::Colon))
            .then(type_.clone())
            .separated_by(just(Token::Comma))
            .collect()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        just(Token::Colon).ignore_then(type_),
        just(Token::Equals).ignore_then(expr),
    ))
    .map_with(|(ident, args, ret, body), e| Fn {
        ident,
        args,
        ret,
        body,
        span: e.span(),
    });

    let object = group((
        just(Token::Object).ignore_then(ident),
        fn_.repeated()
            .collect()
            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
    ))
    .map_with(|(ident, fns), e| Object {
        ident,
        fns,
        span: e.span(),
    });

    let program = object.repeated().collect().map_with(|objects, e| Program {
        objects,
        span: e.span(),
    });

    program
}

pub fn parse<'src, I>(src: I) -> Result<Program<'src>, Vec<Error<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    parser().parse(src).into_result()
}
