use crate::error::Error;
use crate::tokenizer::Token;
use chumsky::{input::ValueInput, prelude::*};
use derive_more::{Deref, Display};

#[derive(Debug, Clone, Default, PartialEq, Eq, Deref, Display)]
pub struct Ident(String);

pub type Literal = crate::tokenizer::Literal;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FnCall {
    pub ident: Ident,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Literal(Literal),
    FnCall(FnCall),
    Ident(Ident),
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Type {
    pub ident: Ident,
    pub generics: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assign {
    pub ident: Ident,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
    Assign(Assign),
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Fn {
    pub ident: Ident,
    pub args: Vec<(Ident, Type)>,
    pub ret: Type,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Object {
    pub ident: Ident,
    pub fns: Vec<Fn>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Program {
    pub objects: Vec<Object>,
}

pub fn parser<'src, I>() -> impl Parser<'src, I, Program, extra::Err<Rich<'src, Token>>>
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    let ident = select!(Token::Identifier(i) => Ident(i));
    let expr = recursive(|expr| {
        let literal = select!(Token::Literal(l) => l);
        let fn_call = group((
            ident,
            expr.clone()
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        ))
        .map(|(ident, args)| FnCall { ident, args });
        let expr = choice((
            fn_call.map(Expr::FnCall),
            ident.map(Expr::Ident),
            literal.map(Expr::Literal),
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
        .map(|(ident, generics)| Type {
            ident,
            generics: generics.unwrap_or_default(),
        })
    });

    let assign = just(Token::Val)
        .ignore_then(ident)
        .then_ignore(just(Token::Equals))
        .then(expr.clone())
        .map(|(ident, expr)| Assign { ident, expr });

    let stmt = choice((assign.map(Stmt::Assign), expr.map(Stmt::Expr)));

    let fn_ = group((
        just(Token::Def).ignore_then(ident),
        ident
            .then_ignore(just(Token::Colon))
            .then(type_.clone())
            .separated_by(just(Token::Comma))
            .collect()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        just(Token::Colon).ignore_then(type_),
        just(Token::Equals).ignore_then(
            stmt.repeated()
                .collect()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        ),
    ))
    .map(|(ident, args, ret, body)| Fn {
        ident,
        args,
        ret,
        body,
    });

    let object = group((
        just(Token::Object).ignore_then(ident),
        fn_.repeated()
            .collect()
            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
    ))
    .map(|(ident, fns)| Object { ident, fns });

    let program = object
        .repeated()
        .collect()
        .map(|objects| Program { objects });

    program
}

pub fn parse<'src, I>(src: I) -> Result<Program, Vec<Error<'src, Token>>>
where
    I: ValueInput<'src, Token = Token, Span = SimpleSpan>,
{
    parser().parse(src).into_result()
}
