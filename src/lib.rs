//! # rsedn - Rust EDN parser
//! 
//! `rsedn` is a parser for the EDN data format written in Rust.
//! 
//! ## Example
//! ```rust
//! use std::collections::LinkedList;
//! 
//! fn main() {
//!     // A Source can be created from a &str
//!     let mut source = "(defn add [a b] (+ a b))".into::<rsedn::Source>(); 
//!     // Lex the source into Vec<Lexeme>
//!     let lexemes = source.lex();
//!    // Parse the lexemes into a LinkedList<Token>
//!     let tokens = lexemes
//!         .into_iter()
//!         .map(|lexeme| Token::parse(&source, lexeme)) // Parse the lexeme into a Token
//!         .map(|token| token.unwrap()) // Unwrap the Result<Token, ParsingError>
//!         .collect::<LinkedList<_>>();
//!     let mut token_stream = tokens.iter(); // Create a TokenStream from the LinkedList
//!     let form = parse_tokens(&mut token_stream).unwrap().unwrap(); // Parse the tokens into a Form
//!     
//!     assert!(matches!(form.kind, FormKind::List(_)));
//! }
//! ```
//! 
//! ## Usage
//! 
//! 1. Take your source code as a `&str`
//! 2. Create a [`Source`] from the `&str` using `rsedn::Source::from`
//! 3. Lex the `Source` using [`Source::lex`] this produces a `Vec<Lexeme>`
//! 4. Parse each [`Lexeme`] into a [`Token`] using `Token::parse`
//! 5. Collect the `Token`s into a `LinkedList<Token>`
//! 6. Create a `TokenStream` from the `LinkedList<Token>` using `LinkedList::iter`
//! 7. Consume the `TokenStream` using [`parse_tokens`] to produce a `Result<Option<Form>, ParsingError>`
//! 8. Use the `Source` and the `Lexeme` to get the span of a given `Lexeme` 

use lexer::{Lexeme, Source};
use parser::{Form, ParsingError};
use token::{Token, TokenStream, TokenizationError};

pub mod lexer;
pub mod parser;
pub mod token;
pub mod error;

/// Produces a [`Source`] from a `&str`
/// The first step of the parsing process
pub fn source_from_str<'source>(source: &'source str) -> Source<'source> {
    source.into()
}

/// Lexes a [`Source`] into a `Vec<Lexeme>`
/// The second step of the parsing process
pub fn lex_source(source: &mut Source<'_>) -> Vec<Lexeme> {
    source.lex()
}

/// Parses a [`Lexeme`] into a [`Token`] using the [`Source`] to get the span
/// The third step of the parsing process
pub fn parse_lexeme<'source>(source: &'source Source<'_>, lexeme: Lexeme) -> Result<Token<'source>, TokenizationError> {
    Token::parse(source, lexeme)
}

/// Consumes a [`TokenStream`] to produce a `Result<Option<Form>, ParsingError>`
/// The final step of the parsing process
pub fn consume_token_stream<'source>(token_stream: &mut TokenStream<'source>) -> Result<Option<Form<'source>>, ParsingError<'source>> {
    parser::parse_form(token_stream)
}
