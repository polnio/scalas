use chumsky::prelude::*;
use derive_more::Display;

use crate::error::{Error, Spanned};

#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum Literal {
    Number(i64),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum Token {
    Literal(Literal),
    Identifier(String),

    // Keywords
    #[display("object")]
    Object,
    #[display("def")]
    Def,
    #[display("val")]
    Val,

    // Symbols
    #[display(":")]
    Colon,
    #[display(";")]
    Semicolon,
    #[display(",")]
    Comma,
    #[display("=")]
    Equals,
    #[display("(")]
    LeftParen,
    #[display(")")]
    RightParen,
    #[display("{{")]
    LeftBrace,
    #[display("}}")]
    RightBrace,
    #[display("[")]
    LeftBracket,
    #[display("]")]
    RightBracket,
}

fn tokenizer<'a>() -> impl Parser<'a, &'a str, Vec<Spanned<'a, Token>>, extra::Err<Error<'a, char>>>
{
    choice((
        text::ident().map(|i| match i {
            "object" => Token::Object,
            "def" => Token::Def,
            "val" => Token::Val,
            _ => Token::Identifier(i.to_owned()),
        }),
        just(':').to(Token::Colon),
        just(';').to(Token::Semicolon),
        just(',').to(Token::Comma),
        just('=').to(Token::Equals),
        just('(').to(Token::LeftParen),
        just(')').to(Token::RightParen),
        just('{').to(Token::LeftBrace),
        just('}').to(Token::RightBrace),
        just('[').to(Token::LeftBracket),
        just(']').to(Token::RightBracket),
        text::int(10)
            .from_str()
            .unwrapped()
            .map(|n| Token::Literal(Literal::Number(n))),
        none_of('"')
            .repeated()
            .collect()
            .delimited_by(just('"'), just('"'))
            .map(|s| Token::Literal(Literal::String(s))),
    ))
    .map_with(|t, e| (t, e.span()))
    .padded_by(text::whitespace())
    .repeated()
    .collect()
}

pub fn tokenize<'a>(src: &'a str) -> Result<Vec<Spanned<'a, Token>>, Vec<Error<'a, char>>> {
    tokenizer().parse(src).into_result()
}
