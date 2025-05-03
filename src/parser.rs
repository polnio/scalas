use crate::builtins::BUILTINS_IDENTIFIERS;
use crate::error::Error;
use crate::tokenizer::Token;
use chumsky::extra::SimpleState;
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use derive_more::{Deref, Display};

#[derive(Debug, Clone, Default, PartialEq, Eq, Deref, Display)]
pub struct Ident<'src>(&'src str);

pub type Literal<'src> = crate::tokenizer::Literal<'src>;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FnCall<'src> {
    pub ident: Ident<'src>,
    pub args: Vec<Expr<'src>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<'src> {
    Literal(Literal<'src>),
    FnCall(FnCall<'src>),
    Ident(Ident<'src>),
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Type<'src> {
    pub ident: Ident<'src>,
    pub generics: Vec<Type<'src>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assign<'src> {
    pub ident: Ident<'src>,
    pub expr: Expr<'src>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt<'src> {
    Expr(Expr<'src>),
    Assign(Assign<'src>),
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Fn<'src> {
    pub ident: Ident<'src>,
    pub args: Vec<(Ident<'src>, Type<'src>)>,
    pub ret: Type<'src>,
    pub body: Vec<Stmt<'src>>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Object<'src> {
    pub ident: Ident<'src>,
    pub fns: Vec<Fn<'src>>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Program<'src> {
    pub objects: Vec<Object<'src>>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParserState<'src> {
    variables: Vec<Ident<'src>>,
}

pub fn parser<'src, I>() -> impl Parser<
    'src,
    I,
    Program<'src>,
    extra::Full<Error<'src, Token<'src>>, SimpleState<ParserState<'src>>, ()>,
>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
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
        .ignore_then(ident.validate(|ident, extra, e| {
            let span = extra.span();
            let state: &mut SimpleState<ParserState> = extra.state();
            if state.variables.contains(&ident) {
                e.emit(Error::custom(span, "Variable already defined"));
            } else {
                state.variables.push(ident.clone());
            }
            ident
        }))
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

pub fn parse<'src, I>(src: I) -> Result<Program<'src>, Vec<Error<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let mut state = ParserState::default();
    state
        .variables
        .extend(BUILTINS_IDENTIFIERS.iter().map(|s| Ident(*s)));
    parser()
        .parse_with_state(src, &mut SimpleState(state))
        .into_result()
}
