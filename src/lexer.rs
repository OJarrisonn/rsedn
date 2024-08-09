//! This is the lexer module. Source code is read as a `Chars` iterator and and it is tokenized into `Token`.

use std::str::Chars;

const DELIMITERS: &str = "()[]{}";
const WHITESPACE: &str = " \t\n\r,";

/// The lexer struct. It's reponsible for generating a vector of `Lexeme` from source code (`&str`).
/// 
/// Can be also used to get the span of a `Lexeme` in the source code.
#[derive(Debug, Clone)]
pub struct Lexer<'source> {
    chars: Chars<'source>,
    source: &'source str,
    current: usize
}

/// A `Lexeme` is a span of the source code. It's represented by a start and end index (consider the source code as an array of bytes).
/// A `Lexeme` isn't categorized, just a piece of the source code.
#[derive(Debug, Clone, Copy)]
pub struct Lexeme(usize, usize);

impl<'source> Lexer<'source> {
    /// Lex the source code and return a vector of `Lexeme`.
    /// This won't treat any errors nor classify the lexemes.
    pub fn lex(&mut self) -> Vec<Lexeme> {
        let mut lexemes = vec![];

        loop {
            match self.peek() {
                None => break,
                Some(c) if DELIMITERS.contains(c) => lexemes.push(self.consume().expect("There should be a delimiter character here to be consumed")),
                Some(c) if WHITESPACE.contains(c) => self.ignore_whitespace(),
                Some('"') => lexemes.push(self.consume_string().expect("There should be a valid string lexeme here")),
                _ => lexemes.push(self.consume_lexeme().expect("There should be a valid lexeme here to be consumed")),
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
        let next = self.chars.next();

        if let Some(c) = next {
            self.current += c.len_utf8();
            Some(Lexeme(start, self.current))
        } else {
            None
        }
    }

    /// Consume characters while the predicate is true producing a `Lexeme`.
    fn consume_while<F: Fn(char) -> bool>(&mut self, f: F) -> Option<Lexeme> {
        let start = self.current;

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
            Some(Lexeme(start, self.current))
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

        // Check if it's `"` started
        if let Some('"') = self.peek() {
            self.consume();
        } else {
            return None;
        }

        while let Some(c) = self.peek() {
            match c {
                '"' => { self.consume(); break; },
                '\\' => {
                    self.consume();
                    self.consume();
                },
                _ => { self.consume(); }
            }
        }

        Some(Lexeme(start, self.current))
    }

    /// Get the span of a `Lexeme` in the source code.
    fn span(&self, lexeme: Lexeme) -> &'source str {
        &self.source[lexeme.0..lexeme.1]
    }
}

impl<'source> From<&'source str> for Lexer<'source> {
    fn from(value: &'source str) -> Self {
        Self {
            chars: value.chars(),
            source: value,
            current: 0,
        }
    }
}

impl Lexeme {
    /// Get the span of the `Lexeme` in the source code.
    pub fn as_str<'source>(&self, source: &Lexer<'source>) -> &'source str {
        &source.source[self.0..self.1]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ignore_whitespace() {
        let mut source: Lexer = "  \t\n\r,()[]{}'\"abc".into();
        
        source.ignore_whitespace();
        
        assert_eq!(&source.source[source.current..], "()[]{}'\"abc");
    }

    #[test]
    fn lexemes_no_string() {
        let mut source: Lexer = "(defn hello 123)".into();
        let lexemes = source.lex();
        let tokens = lexemes.iter().map(|l| l.as_str(&source)).collect::<Vec<&str>>();
        assert_eq!(tokens, vec!["(", "defn", "hello", "123", ")"])
 
    }

    #[test]
    fn string_lexemes() {
        let mut source: Lexer = "(def msg \"Hello World\")".into();
        let lexemes = source.lex();
        let tokens = lexemes.iter().map(|l| l.as_str(&source)).collect::<Vec<&str>>();
        assert_eq!(tokens, vec!["(", "def", "msg", "\"Hello World\"", ")"])
    }
}