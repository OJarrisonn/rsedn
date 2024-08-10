use std::{
    collections::linked_list::Iter,
    error::Error,
    fmt::{self, Display, Formatter},
};

use crate::lexer::{Lexeme, Source};

const SYMBOL_CONSTITUENT: &str = ".*+!-_?$%&=<>:#";
const SYMBOL_STARTER: &str = ".*+!-_?$%&=<>";

pub type TokenStream<'source> = Iter<'source, Token<'source>>;

/// A token in the source code.
/// This contains the kind of token and the lexeme that represents it.
#[derive(Debug, Clone, PartialEq)]
pub struct Token<'source> {
    pub kind: TokenKind<'source>,
    pub lexeme: Lexeme,
}

/// The kind of token.
/// This classifies the token into a specific kind.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'source> {
    Nil,
    OpenParen,
    CloseParen,
    OpenHashBrace,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Boolean(bool),
    String(String),
    Character(char),
    Symbol(&'source str),
    Keyword(&'source str),
    Tag(&'source str),
    Discard(&'source str),
    Integer(i64),
    Float(f64),
}

/// The error that can happen during tokenization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenizationError {
    ArbitraryPrecisionInteger(Lexeme),
    UnicodeEscapeSequence(Lexeme),
    UnknownSequence(Lexeme),
}

impl<'source> Token<'source> {
    pub fn parse(
        source: &Source<'source>,
        lexeme: Lexeme,
    ) -> Result<Token<'source>, TokenizationError> {
        let span = source.span(lexeme);
        let kind = match span {
            "(" => Ok(TokenKind::OpenParen),
            ")" => Ok(TokenKind::CloseParen),
            "#{" => Ok(TokenKind::OpenHashBrace),
            "{" => Ok(TokenKind::OpenBrace),
            "}" => Ok(TokenKind::CloseBrace),
            "[" => Ok(TokenKind::OpenBracket),
            "]" => Ok(TokenKind::CloseBracket),
            "nil" => Ok(TokenKind::Nil),
            "true" => Ok(TokenKind::Boolean(true)),
            "false" => Ok(TokenKind::Boolean(false)),
            span if is_string(span) => {
                if span.contains("\\u") {
                    Err(TokenizationError::UnicodeEscapeSequence(lexeme))
                } else {
                    Ok(TokenKind::String(parse_string(span)))
                }
            }
            span if is_integer_n(span) => Err(TokenizationError::ArbitraryPrecisionInteger(lexeme)),
            span if is_integer_m(span) => Ok(TokenKind::Float(parse_integer_m(span))),
            span if is_integer(span) => Ok(TokenKind::Integer(
                span.parse()
                    .expect(&format!("This integer should be valid `{}`", span)),
            )),
            span if is_float(span) => Ok(TokenKind::Float(
                span.parse()
                    .expect(&format!("This float should be valid `{}`", span)),
            )),
            span if is_character(span) => Ok(TokenKind::Character(parse_character(span))),
            span if is_keyword(span) => Ok(TokenKind::Keyword(&span[1..])),
            span if is_symbol(span) => Ok(TokenKind::Symbol(span)),
            span if is_tag(span) => Ok(TokenKind::Tag(&span[1..])),
            span if is_discard(span) => Ok(TokenKind::Discard(&span[2..])),
            _ => Err(TokenizationError::UnknownSequence(lexeme)),
        };

        kind.map(|kind| Token { kind, lexeme })
    }

    pub fn is_terminal(&self) -> bool {
        match self.kind {
            TokenKind::Nil
            | TokenKind::Boolean(_)
            | TokenKind::String(_)
            | TokenKind::Character(_)
            | TokenKind::Symbol(_)
            | TokenKind::Keyword(_)
            | TokenKind::Tag(_)
            | TokenKind::Discard(_)
            | TokenKind::Integer(_)
            | TokenKind::Float(_) => true,
            _ => false,
        }
    }
}

/// Check if the span is a string.
fn is_string(span: &str) -> bool {
    span.starts_with('"')
        && span.ends_with('"')
        && span.len() > 1
        && span.chars().nth_back(1) != Some('\\')
}

/// Parse a string span into an actual string.
/// Handle scape sequences
fn parse_string(span: &str) -> String {
    span[1..span.len() - 1]
        .replace("\\\"", "\"")
        .replace("\\n", "\n")
        .replace("\\r", "\r")
        .replace("\\t", "\t")
        .replace("\\\\", "\\")
    // TODO: \uXXXX and \UXXXXXXXX
}

/// Check if the span is a character.
fn is_character(span: &str) -> bool {
    span.starts_with('\\') && span.len() == 2
        || span == "\\space"
        || span == "\\newline"
        || span == "\\tab"
        || span == "\\return"
}

/// Parse a character span into an actual character.
fn parse_character(span: &str) -> char {
    match span {
        "\\space" => ' ',
        "\\newline" => '\n',
        "\\tab" => '\t',
        "\\return" => '\r',
        _ => span.chars().nth(1).unwrap(),
    }
}

/// Check if the span is an integer.
/// TODO: Expand number notion
fn is_integer(span: &str) -> bool {
    let mut chars = span.chars();
    match chars.next() {
        Some('-') | Some('+') => span.len() > 1 && chars.all(|c| c.is_digit(10)),
        Some('0') => chars.next().is_none(),
        Some(c) if c.is_digit(10) => chars.all(|c| c.is_digit(10)),
        _ => false,
    }
}

/// Check if the span is an integer with arbitrary precision.
fn is_integer_n(span: &str) -> bool {
    span.ends_with('N') && is_integer(&span[..span.len() - 1])
}

/// Check if the span is an integer with exact precision.
fn is_integer_m(span: &str) -> bool {
    span.ends_with('M') && is_integer(&span[..span.len() - 1])
}

fn parse_integer_m(span: &str) -> f64 {
    span[..span.len() - 1].parse().unwrap()
}

/// Check if the span is a float.
/// TODO: Refactor this function
fn is_float(span: &str) -> bool {
    let matches = span
        .chars()
        .all(|c| c.is_digit(10) || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+')
        && span.matches('.').count() <= 1
        && span.matches('e').count() <= 1
        && span.matches('E').count() <= 1
        && span.matches('-').count() <= 1
        && span.matches('+').count() <= 1;
    if !matches {
        return false;
    }

    let span = span.to_lowercase();
    let parts: Vec<&str> = span.split('.').collect();

    if parts.len() == 2 {
        let integer = parts[0];
        let decimal = parts[1];

        if !is_integer(integer) {
            return false;
        }
        if decimal.chars().all(|c| c.is_digit(10)) {
            return true;
        } else {
            let parts = decimal.split('e').collect::<Vec<&str>>();
            if parts.len() == 2 {
                let mantissa = parts[0];
                let exponent = parts[1];
                if !mantissa.chars().all(|c| c.is_digit(10)) {
                    return false;
                }
                if !is_integer(exponent) {
                    return false;
                }
                return true;
            } else {
                let mantissa = parts[0];

                mantissa.chars().all(|c| c.is_digit(10))
            }
        }
    } else if parts.len() == 1 {
        let parts = span.split('e').collect::<Vec<&str>>();
        if parts.len() == 2 {
            let mantissa = parts[0];
            let exponent = parts[1];
            if !mantissa.chars().all(|c| c.is_digit(10)) {
                return false;
            }
            if !is_integer(exponent) {
                return false;
            }
            return true;
        } else {
            let mantissa = parts[0];

            mantissa.chars().all(|c| c.is_digit(10))
        }
    } else {
        false
    }
}

/// Check if the span is a symbol.
fn is_symbol(span: &str) -> bool {
    if span == "/" {
        true
    } else if span.contains("/") {
        let mut parts = span.split("/");
        parts.all(|part| is_symbol_name(part))
    } else {
        is_symbol_name(span)
    }
}

/// Check if the span is a part of a symbol .
fn is_symbol_name(span: &str) -> bool {
    let mut chars = span.chars();
    match chars.next() {
        Some('+') | Some('-') | Some('.') => match chars.next() {
            None => true,
            Some(c) if c.is_alphabetic() || SYMBOL_STARTER.contains(c) => {
                chars.all(|c| c.is_alphanumeric() || SYMBOL_CONSTITUENT.contains(c))
            }
            _ => false,
        },
        Some(c) if c.is_alphabetic() || SYMBOL_STARTER.contains(c) => {
            chars.all(|c| c.is_alphanumeric() || SYMBOL_CONSTITUENT.contains(c))
        }
        _ => false,
    }
}

/// Check if the span is a keyword.
fn is_keyword(span: &str) -> bool {
    span.starts_with(':') && is_symbol(&span[1..])
}

/// Check if the span is a tag.
fn is_tag(span: &str) -> bool {
    span.starts_with('#') && is_symbol(&span[1..])
}

/// Check if the span is a discard.
fn is_discard(span: &str) -> bool {
    span.starts_with("#_") && is_symbol(&span[2..])
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Nil => write!(f, "nil"),
            TokenKind::OpenParen => write!(f, "("),
            TokenKind::CloseParen => write!(f, ")"),
            TokenKind::OpenHashBrace => write!(f, "#{{"),
            TokenKind::OpenBrace => write!(f, "{{"),
            TokenKind::CloseBrace => write!(f, "}}"),
            TokenKind::OpenBracket => write!(f, "["),
            TokenKind::CloseBracket => write!(f, "]"),
            TokenKind::Boolean(b) => write!(f, "{}", b),
            TokenKind::String(s) => write!(f, "\"{}\"", s),
            TokenKind::Character(c) => write!(f, "\\{}", c),
            TokenKind::Symbol(s) => write!(f, "{}", s),
            TokenKind::Keyword(k) => write!(f, ":{}", k),
            TokenKind::Tag(t) => write!(f, "#{}", t),
            TokenKind::Discard(d) => write!(f, "#_{}", d),
            TokenKind::Integer(i) => write!(f, "{}", i),
            TokenKind::Float(fl) => write!(f, "{}", fl),
        }
    }
}

impl Display for TokenizationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TokenizationError::ArbitraryPrecisionInteger(lexeme) => write!(
                f,
                "Arbitrary precision integer at [{}:{}]",
                lexeme.line(),
                lexeme.column()
            ),
            TokenizationError::UnicodeEscapeSequence(lexeme) => write!(
                f,
                "Unicode escape sequence at [{}:{}]",
                lexeme.line(),
                lexeme.column()
            ),
            TokenizationError::UnknownSequence(lexeme) => write!(
                f,
                "Unknown sequence at [{}:{}]",
                lexeme.line(),
                lexeme.column()
            ),
        }
    }
}

impl Error for TokenizationError {}

#[cfg(test)]
mod tests {
    use crate::token::is_symbol;

    #[test]
    fn symbol_check() {
        assert!(is_symbol("foo"));
        assert!(is_symbol("foo/bar"));
        assert!(is_symbol("foo/bar-baz"));
        assert!(is_symbol("foo/bar-baz!"));
        assert!(is_symbol("+"));
    }
}
