/// A `Lexeme` is a span of the source code. It's represented by a start and end index (consider the source code as an array of bytes).
/// A `Lexeme` isn't categorized, just a piece of the source code.
/// Lexemes can be parsed into [`Token`].
///
/// [`Token`]: token/enum.Token.html
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lexeme {
    /// The starting byte index of the lexeme.
    pub(super) start: usize,
    /// The ending byte index of the lexeme.
    pub(super) end: usize,
    /// The starting line number of the lexeme.
    pub(super) line: usize,
    /// The starting column number of the lexeme.
    pub(super) column: usize,
}

impl Lexeme {
    /// Get the position of the `Lexeme` in the source code as (line, column).
    pub fn position(&self) -> (usize, usize) {
        (self.line, self.column)
    }

    /// Get the starting line number of the `Lexeme`.
    pub fn line(&self) -> usize {
        self.line
    }

    /// Get the starting column number of the `Lexeme`.
    pub fn column(&self) -> usize {
        self.column
    }

    /// Join two `Lexeme` into a single one.
    pub fn join<'source>(&self, other: &Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
            line: self.line,
            column: self.column,
        }
    }
}
