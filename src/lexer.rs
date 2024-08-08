//! This is the lexer module. Source code is read as a `Chars` iterator and and it is tokenized into `Token`.

use std::{iter::Peekable, str::Chars};
use crate::token::Token;

const DELIMITERS: &str = " \t\n\r,()[]{}'\"";
const WHITESPACE: &str = " \t\n\r,";

type Source<'source> = Chars<'source>;

pub fn lex<'source>(source: &'source str) -> Result<Vec<Token<'source>>, String> {
    let mut source: Source = source.chars();
    let mut tokens = Vec::new();

    loop {
        ignore_whitespace(&mut source);
        let raw = lex_raw_token(&mut source);
    }

    Ok(tokens)
}

/// This function gets a &str from chars until a delimiter is found.
fn lex_raw_token<'source>(source: &mut Source<'source>) -> &'source str {
    let start = source.as_str();
    let mut count: usize = 0;
    while let Some(c) = source.clone().next() {
        if DELIMITERS.contains(c) {
            break;
        }
        source.next();
        count += c.len_utf8();
    }
    &start[..count]
}

/// This expects the function to be called after a delimiter is found.
fn lex_delimiter_token<'source>(source: &mut Source<'source>) -> Token<'source> {
    let c = source.next().unwrap();
    match c {
        '(' => Token::OpenParen,
        ')' => Token::CloseParen,
        '[' => Token::OpenBracket,
        ']' => Token::CloseBracket,
        '{' => Token::OpenBrace,
        '}' => Token::CloseBrace,
        // TODO: Treat `#` 
        _ => panic!("Unexpected char: {}", c),
    }
}

/// Advances chars until a non-whitespace character is found.
fn ignore_whitespace<'source>(source: &mut Source<'source>) {
    while let Some(c) = source.clone().next() {
        if !WHITESPACE.contains(c) { 
            break; 
        }
        source.next();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ignore_whitespace() {
        let mut source: Source = "  \t\n\r,()[]{}'\"abc".chars();
        
        ignore_whitespace(&mut source);
        
        assert_eq!(source.as_str(), "()[]{}'\"abc");
    }

    #[test]
    fn test_lex_a_token() {
        let mut source: Source = "abc 123".chars();
        
        let token = lex_raw_token(&mut source);
        
        assert_eq!(token, "abc");
    }

    #[test]
    fn test_lex_a_token_with_delimiter() {
        let mut source: Source = "abc, 123".chars();
        
        let token = lex_raw_token(&mut source);
        
        assert_eq!(token, "abc");
    }

    #[test]
    fn test_lex_a_delimiter_as_token() {
        let mut source: Source = "()".chars();
        
        let token = lex_raw_token(&mut source);
        
        assert_eq!(token, "(");
    }
}