//! This is the lexer module. Source code is read as a `Chars` iterator and and it is tokenized into `Token`.

use std::str::Chars;

const DELIMITERS: &str = "()[]{}";
const WHITESPACE: &str = " \t\n\r,";

#[derive(Debug, Clone)]
pub struct Source<'source> {
    chars: Chars<'source>,
    source: &'source str,
    current: usize
}

#[derive(Debug, Clone, Copy)]
pub struct Lexeme(usize, usize);

impl<'source> Source<'source> {
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

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

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

    fn ignore_whitespace(&mut self) {
        self.consume_while(|c| WHITESPACE.contains(c));
    }

    fn consume_lexeme(&mut self) -> Option<Lexeme> {
        self.consume_while(|c| !DELIMITERS.contains(c) && !WHITESPACE.contains(c))
    }

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
                '"' => { break; },
                '\\' => {
                    self.consume();
                    self.consume();
                },
                _ => { self.consume(); }
            }
        }

        // Check if it's `"` ended
        if let Some('"') = self.peek() {
            self.consume();
            Some(Lexeme(start, self.current))
        } else {
            None
        }
    }
}

impl<'source> From<&'source str> for Source<'source> {
    fn from(value: &'source str) -> Self {
        Self {
            chars: value.chars(),
            source: value,
            current: 0,
        }
    }
}

impl Lexeme {
    pub fn as_str<'source>(&self, source: &Source<'source>) -> &'source str {
        &source.source[self.0..self.1]
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
        let tokens = lexemes.iter().map(|l| l.as_str(&source)).collect::<Vec<&str>>();
        assert_eq!(tokens, vec!["(", "defn", "hello", "123", ")"])
    }
}