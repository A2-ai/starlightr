#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: impl Into<usize>, end: impl Into<usize>) -> Self {
        Self {
            start: start.into(),
            end: end.into(),
        }
    }

    /// Convert a byte offset to a 1-based (line, column) pair.
    pub fn line_col(source: &str, offset: usize) -> (usize, usize) {
        let offset = offset.min(source.len());
        let mut line = 1;
        let mut col = 1;
        for (i, ch) in source.char_indices() {
            if i >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        (line, col)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self {
            value,
            span,
        }
    }
}
