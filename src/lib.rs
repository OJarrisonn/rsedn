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
mod lexer;
mod parser;
mod token;
pub mod error;

pub use lexer::{Source, Lexeme};
pub use token::{Token, TokenKind, TokenStream};
pub use parser::{Form, FormKind, ParsingError, parse_tokens};