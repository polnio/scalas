use ariadne::Report;
use chumsky::error::Rich;
use chumsky::span::SimpleSpan;
use std::fmt::Display;

pub type Spanned<T> = (T, SimpleSpan);
pub type Error<'a, E> = Rich<'a, E>;

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
    .print(ariadne::sources([(path, src)]))
    .unwrap();
}
