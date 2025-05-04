use crate::error::{Error, Spanned};
use chumsky::prelude::*;
use derive_more::Display;

#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum Literal<'src> {
    Number(i64),
    String(&'src str),
}

#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum Token<'src> {
    Literal(Literal<'src>),
    Identifier(&'src str),

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

fn tokenizer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Error<'src, char>>> {
    choice((
        text::ident().map(|i| match i {
            "object" => Token::Object,
            "def" => Token::Def,
            "val" => Token::Val,
            _ => Token::Identifier(i),
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
            .to_slice()
            .delimited_by(just('"'), just('"'))
            .map(|s| Token::Literal(Literal::String(s))),
    ))
    .map_with(|o, e| (o, e.span()))
    .padded_by(text::whitespace())
    .repeated()
    .collect()
}

pub fn tokenize<'src>(src: &'src str) -> Result<Vec<Spanned<Token<'src>>>, Vec<Error<'src, char>>> {
    tokenizer().parse(src).into_result()
}
