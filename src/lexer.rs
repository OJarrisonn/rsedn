//! This is the lexer module. Source code is read as a `Chars` iterator and and it is tokenized into `Token`.

pub mod lexeme;
pub mod source;
pub mod token;

const DELIMITERS: &str = "()[]{}";
const WHITESPACE: &str = " \t\n\r,";
