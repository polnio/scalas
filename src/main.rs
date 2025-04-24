use ariadne::Report;
use chumsky::prelude::*;
use derive_more::Deref;

#[derive(Debug, Clone, Default, PartialEq, Eq, Deref)]
struct Ident(String);

#[derive(Debug, Clone, PartialEq, Eq)]
enum Literal {
    Number(i64),
    String(String),
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct FnCall {
    ident: Ident,
    args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Literal(Literal),
    FnCall(FnCall),
    Ident(Ident),
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Type {
    ident: Ident,
    generics: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Stmt {
    Expr(Expr),
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Fn {
    ident: Ident,
    args: Vec<(Ident, Type)>,
    ret: Type,
    body: Vec<Stmt>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Object {
    ident: Ident,
    fns: Vec<Fn>,
}

fn parser<'a>() -> impl Parser<'a, &'a str, Object, extra::Err<Rich<'a, char>>> {
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

    let stmt = choice((expr.map(Stmt::Expr),)).padded();

    let fn_ = group((
        just("def ").ignore_then(ident),
        ident
            .then_ignore(just(':'))
            .then(type_.clone())
            .separated_by(just(','))
            .collect()
            .delimited_by(just('('), just(')')),
        just(':').padded().ignore_then(type_),
        just('=')
            .padded()
            .ignore_then(stmt.repeated().collect().delimited_by(just('{'), just('}'))),
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

    object
}

fn main() {
    let mut args = std::env::args();
    let exe = args.next().unwrap();
    let Some(path) = args.next() else {
        eprintln!("Usage: {} <file>", exe);
        std::process::exit(1);
    };
    let src = match std::fs::read_to_string(&path) {
        Ok(src) => src,
        Err(err) => {
            eprintln!("Error reading file {}: {}", path, err);
            std::process::exit(1);
        }
    };
    let result = match parser().parse(&src).into_result() {
        Ok(result) => result,
        Err(errs) => {
            for err in errs {
                Report::build(
                    ariadne::ReportKind::Error,
                    (path.clone(), err.span().into_range()),
                )
                .with_label(
                    ariadne::Label::new((path.clone(), err.span().into_range()))
                        .with_message(err.reason().to_string())
                        .with_color(ariadne::Color::Red),
                )
                .finish()
                .print(ariadne::sources([(path.clone(), &src)]))
                .unwrap();
            }
            std::process::exit(1);
        }
    };
    dbg!(result);
}
