use chumsky::prelude::*;
use derive_more::{Deref, Display};

#[derive(Debug, Clone, Default, PartialEq, Eq, Deref, Display)]
pub struct Ident(String);

#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum Literal {
    Number(i64),
    String(String),
}

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

pub fn parser<'a>() -> impl Parser<'a, &'a str, Program, extra::Err<Rich<'a, char>>> {
    let ident = text::ascii::ident().map(|i: &str| Ident(i.to_owned()));
    let expr = recursive(|expr| {
        let literal = choice((
            text::int(10).from_str().unwrapped().map(Literal::Number),
            none_of('"')
                .repeated()
                .collect()
                .delimited_by(just('"'), just('"'))
                .map(Literal::String),
        ));
        let fn_call = group((
            ident,
            expr.clone()
                .separated_by(just(','))
                .collect()
                .delimited_by(just('('), just(')')),
        ))
        .map(|(ident, args)| FnCall { ident, args });
        let expr = choice((
            fn_call.map(Expr::FnCall),
            ident.map(Expr::Ident),
            literal.map(Expr::Literal),
        ))
        .padded();
        expr
    });

    let type_ = recursive(|type_| {
        group((
            ident,
            type_
                .separated_by(just(','))
                .collect()
                .delimited_by(just('['), just(']'))
                .or_not(),
        ))
        .padded()
        .map(|(ident, generics)| Type {
            ident,
            generics: generics.unwrap_or_default(),
        })
    });

    let assign = just("val ")
        .ignore_then(ident)
        .then_ignore(just('=').padded())
        .then(expr.clone())
        .map(|(ident, expr)| Assign { ident, expr });

    let stmt = choice((assign.map(Stmt::Assign), expr.map(Stmt::Expr))).padded();

    let fn_ = group((
        just("def ").ignore_then(ident),
        ident
            .then_ignore(just(':'))
            .then(type_.clone())
            .separated_by(just(','))
            .collect()
            .delimited_by(just('('), just(')')),
        just(':').padded().ignore_then(type_),
        just('=').padded().ignore_then(
            stmt.repeated()
                .collect()
                .delimited_by(just('{').padded(), just('}').padded()),
        ),
    ))
    .padded()
    .map(|(ident, args, ret, body)| Fn {
        ident,
        args,
        ret,
        body,
    });

    let object = group((
        just("object").padded().ignore_then(ident),
        fn_.repeated()
            .collect()
            .delimited_by(just('{'), just('}'))
            .padded(),
    ))
    .map(|(ident, fns)| Object { ident, fns });

    let program = object
        .repeated()
        .collect()
        .map(|objects| Program { objects });

    program
}
