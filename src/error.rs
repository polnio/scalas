use ariadne::Report;
use chumsky::error::Rich;

pub fn report(path: String, src: String, err: Rich<'_, char>) {
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
