//! This is the lexer module. Source code is read as a `Chars` iterator and and it is tokenized into `Token`.

use std::str::Chars;

const DELIMITERS: &str = "()[]{}";
const WHITESPACE: &str = " \t\n\r,";

/// The source struct. It's reponsible for generating a vector of `Lexeme` from source code (`&str`).
///
/// Can be also used to get the span of a `Lexeme` in the source code.
///
/// It's important to keep this struct and the source text alive as long as all your AST is alive
#[derive(Debug, Clone)]
pub struct Source<'source> {
    /// The source code as a `Chars` iterator.
    chars: Chars<'source>,
    /// The source code to be lexed.
    source: &'source str,
    /// The current byte index in the source code.
    current: usize,
    /// The line number of the current char
    line: usize,
    /// The column number of the current char
    column: usize,
}

/// A `Lexeme` is a span of the source code. It's represented by a start and end index (consider the source code as an array of bytes).
/// A `Lexeme` isn't categorized, just a piece of the source code.
/// Lexemes can be parsed into [`Token`].
///
/// [`Token`]: token/enum.Token.html
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lexeme {
    /// The starting byte index of the lexeme.
    start: usize,
    /// The ending byte index of the lexeme.
    end: usize,
    /// The starting line number of the lexeme.
    line: usize,
    /// The starting column number of the lexeme.
    column: usize,
}

impl<'source> Source<'source> {
    /// Lex the source code and return a vector of `Lexeme`.
    /// This won't treat any errors nor classify the lexemes.
    pub fn lex(&mut self) -> Vec<Lexeme> {
        let mut lexemes = vec![];

        loop {
            match self.peek() {
                None => break,
                Some(c) if DELIMITERS.contains(c) => lexemes.push(
                    self.consume()
                        .expect("There should be a delimiter character here to be consumed"),
                ),
                Some(c) if WHITESPACE.contains(c) => self.ignore_whitespace(),
                Some('"') => lexemes.push(
                    self.consume_string()
                        .expect("There should be a valid string lexeme here"),
                ),
                Some(';') => {
                    self.consume_while(|c| c != '\n');
                }
                _ => lexemes.push(
                    self.consume_lexeme()
                        .expect("There should be a valid lexeme here to be consumed"),
                ),
            }
        }

        lexemes
    }

    /// Peek the next character in the source code without advancing the reading cursor.
    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    /// Consume the next character as a lexeme in the source code and advance the reading cursor.
    fn consume(&mut self) -> Option<Lexeme> {
        let start = self.current;
        let line = self.line;
        let column = self.column;
        let next = self.chars.next();

        if let Some(c) = next {
            self.current += c.len_utf8();

            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }

            Some(Lexeme {
                start,
                end: self.current,
                line,
                column,
            })
        } else {
            None
        }
    }

    /// Consume characters while the predicate is true producing a `Lexeme`.
    fn consume_while<F: Fn(char) -> bool>(&mut self, f: F) -> Option<Lexeme> {
        let start = self.current;
        let line = self.line;
        let column = self.column;

        while let Some(c) = self.peek() {
            if f(c) {
                self.consume();
            } else {
                break;
            }
        }

        if start == self.current {
            None
        } else {
            Some(Lexeme {
                start,
                end: self.current,
                line,
                column,
            })
        }
    }

    /// Advances the reading cursor while the character is a whitespace.
    fn ignore_whitespace(&mut self) {
        self.consume_while(|c| WHITESPACE.contains(c));
    }

    /// Consume characters while the character is not a delimiter nor a whitespace producing a `Lexeme`.
    fn consume_lexeme(&mut self) -> Option<Lexeme> {
        self.consume_while(|c| !DELIMITERS.contains(c) && !WHITESPACE.contains(c))
    }

    /// Consume a string lexeme which can contain whitespaces.
    fn consume_string(&mut self) -> Option<Lexeme> {
        let start = self.current;
        let line = self.line;
        let column = self.column;

        // Check if it's `"` started
        if let Some('"') = self.peek() {
            self.consume();
        } else {
            return None;
        }

        while let Some(c) = self.peek() {
            match c {
                '"' => {
                    self.consume();
                    break;
                }
                '\\' => {
                    self.consume();
                    self.consume();
                }
                _ => {
                    self.consume();
                }
            }
        }

        Some(Lexeme {
            start,
            end: self.current,
            line,
            column,
        })
    }

    /// Get the span of a `Lexeme` in the source code.
    pub fn span(&self, lexeme: Lexeme) -> &'source str {
        &self.source[lexeme.start..lexeme.end]
    }
}

impl<'source> From<&'source str> for Source<'source> {
    fn from(value: &'source str) -> Self {
        Self {
            chars: value.chars(),
            source: value,
            current: 0,
            line: 1,
            column: 1,
        }
    }
}

impl Lexeme {
    /// Get the span of the `Lexeme` in the source code.
    pub fn as_str<'source>(&self, source: &Source<'source>) -> &'source str {
        &source.source[self.start..self.end]
    }

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ignore_whitespace() {
        let mut source: Source = "  \t\n\r,()[]{}'\"abc".into();

        source.ignore_whitespace();

        assert_eq!(&source.source[source.current..], "()[]{}'\"abc");
    }

    #[test]
    fn lexemes_no_string() {
        let mut source: Source = "(defn hello 123)".into();
        let lexemes = source.lex();
        let tokens = lexemes
            .iter()
            .map(|l| l.as_str(&source))
            .collect::<Vec<&str>>();
        assert_eq!(tokens, vec!["(", "defn", "hello", "123", ")"])
    }

    #[test]
    fn string_lexemes() {
        let mut source: Source = "(def msg \"Hello World\")".into();
        let lexemes = source.lex();
        let tokens = lexemes
            .iter()
            .map(|l| l.as_str(&source))
            .collect::<Vec<&str>>();
        assert_eq!(tokens, vec!["(", "def", "msg", "\"Hello World\"", ")"])
    }

    #[test]
    fn line_count() {
        let mut source: Source = "(defn\nhello\n123)".into();
        let lexemes = source.lex();
        let tokens = lexemes.iter().map(|l| l.position()).collect::<Vec<_>>();
        assert_eq!(tokens, vec![(1, 1), (1, 2), (2, 1), (3, 1), (3, 4)])
    }

    #[test]
    fn commentary() {
        let mut source: Source = ";; A comment\n(defn hello 123) ; Another comment".into();
        let lexemes = source.lex();
        assert_eq!(lexemes
            .iter()
            .map(|l| l.as_str(&source))
            .collect::<Vec<&str>>(),
            vec!["(", "defn", "hello", "123", ")"]);
    }
}
