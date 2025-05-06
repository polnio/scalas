use ariadne::Report;
use chumsky::error::Rich;
use chumsky::span::SimpleSpan;
use std::fmt::Display;

pub type Spanned<T> = (T, SimpleSpan);
pub type Error<'a, E> = Rich<'a, E>;

pub trait AbortStd<T> {
    fn unwrap_with_context(self, context: impl Display) -> T;
}
impl<T, E: std::error::Error> AbortStd<T> for Result<T, E> {
    fn unwrap_with_context(self, context: impl Display) -> T {
        match self {
            Ok(t) => t,
            Err(err) => {
                eprintln!("{}: {}", context, err);
                std::process::exit(1);
            }
        }
    }
}

pub fn report<E: Display>(path: String, src: String, err: Rich<'_, E>) {
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
    .eprint(ariadne::sources([(path, src)]))
    .unwrap();
}
