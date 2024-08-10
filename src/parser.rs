//! Takes tokens and builds an AST of Forms

use std::{error::Error, fmt::Display};

use crate::{
    lexer::Lexeme,
    token::{Token, TokenKind, TokenStream},
};

#[derive(Debug, Clone)]
pub struct Form<'source> {
    pub kind: FormKind<'source>,
    pub lexeme: Lexeme,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FormKind<'source> {
    List(Vec<Form<'source>>),
    Vector(Vec<Form<'source>>),
    Map(Vec<(Form<'source>, Form<'source>)>),
    Set(Vec<Form<'source>>),
    Keyword(&'source str),
    Symbol(&'source str),
    Tag(&'source str),
    Discard(&'source str),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Character(char),
    String(String),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsingError<'source> {
    UnexpectedEOF(Option<Lexeme>),
    UnexpectedToken(Token<'source>),
    UnexpectedTokenExpected(Token<'source>, TokenKind<'source>),
    NonTerminalToken(Token<'source>),
}

impl PartialEq for Form<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

/// Take a token stream and parse it into a Form consuming tokens in the head
/// If the stream is empty, returns None
/// The token stream may not be empty after the call, so it's possible to continue parsing until it returns `Ok(None)`
pub fn parse_tokens<'source>(
    stream: &mut TokenStream<'source>,
) -> Result<Option<Form<'source>>, ParsingError<'source>> {
    let token = match stream.clone().next() {
        Some(token) => token,
        None => return Ok(None),
    };

    match token.kind {
        TokenKind::OpenParen => parse_list(stream).map(Some),
        TokenKind::OpenBracket => parse_vector(stream).map(Some),
        TokenKind::OpenBrace => parse_map(stream).map(Some),
        TokenKind::OpenHashBrace => parse_set(stream).map(Some),
        TokenKind::CloseBracket |
        TokenKind::CloseBrace |
        TokenKind::CloseParen => Err(ParsingError::UnexpectedToken(token.clone())),
        _ if token.is_terminal() => { stream.next(); parse_terminal_token(&token).map(Some) },
        _ => Err(ParsingError::UnexpectedToken(token.clone())),
    }
}

/// Parse a terminal token into a Form
/// Returns an error if the token is not terminal
/// Won't consume the token, nor the token stream
pub fn parse_terminal_token<'source>(token: &Token<'source>) -> Result<Form<'source>, ParsingError<'source>> {
    let kind = match &token.kind {
        TokenKind::Keyword(span) => Ok(FormKind::Keyword(span)),
        TokenKind::Symbol(span) => Ok(FormKind::Symbol(span)),
        TokenKind::Tag(span) => Ok(FormKind::Tag(span)),
        TokenKind::Discard(span) => Ok(FormKind::Discard(span)),
        TokenKind::Integer(span) => Ok(FormKind::Integer(*span)),
        TokenKind::Float(span) => Ok(FormKind::Float(*span)),
        TokenKind::Boolean(span) => Ok(FormKind::Boolean(*span)),
        TokenKind::Character(span) => Ok(FormKind::Character(*span)),
        TokenKind::String(span) => Ok(FormKind::String(span.clone())),
        TokenKind::Nil => Ok(FormKind::Nil),
        _ => Err(ParsingError::NonTerminalToken(token.clone())),
    };

    kind.map(|kind| Form {
        kind,
        lexeme: token.lexeme,
    })
}

pub fn parse_list<'source>(stream: &mut TokenStream<'source>) -> Result<Form<'source>, ParsingError<'source>> {
    let mut forms = Vec::new();

    let start_lexeme = if let Some(token) = stream.next() {
        if token.kind != TokenKind::OpenParen {
            return Err(ParsingError::UnexpectedTokenExpected(token.clone(), TokenKind::OpenParen));
        } else {
            token.lexeme
        }
    } else {
        return Err(ParsingError::UnexpectedEOF(None));
    };

    let mut end_lexeme = start_lexeme;

    loop {
        match stream.clone().next() {
            Some(token) => {
                if token.kind == TokenKind::CloseParen {
                    stream.next();
                    break;
                }

                match parse_tokens(stream) {
                    Ok(Some(form)) => {
                        end_lexeme = form.lexeme;
                        forms.push(form);
                    }
                    Ok(None) => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
                    Err(e) => return Err(e),
                }
            },
            None => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
        }
    }

    Ok(Form {
        kind: FormKind::List(forms),
        lexeme: start_lexeme.join(&end_lexeme),
    })
}

pub fn parse_vector<'source>(stream: &mut TokenStream<'source>) -> Result<Form<'source>, ParsingError<'source>> {
    let mut forms = Vec::new();

    let start_lexeme = if let Some(token) = stream.next() {
        if token.kind != TokenKind::OpenBracket {
            return Err(ParsingError::UnexpectedTokenExpected(token.clone(), TokenKind::OpenBracket));
        } else {
            token.lexeme
        }
    } else {
        return Err(ParsingError::UnexpectedEOF(None));
    };

    let mut end_lexeme = start_lexeme;

    loop {
        match stream.clone().next() {
            Some(token) => {
                if token.kind == TokenKind::CloseBracket {
                    stream.next();
                    break;
                }

                match parse_tokens(stream) {
                    Ok(Some(form)) => {
                        end_lexeme = form.lexeme;
                        forms.push(form);
                    }
                    Ok(None) => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
                    Err(e) => return Err(e),
                }
            },
            None => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
        }
    }

    Ok(Form {
        kind: FormKind::Vector(forms),
        lexeme: start_lexeme.join(&end_lexeme),
    })
}

pub fn parse_map<'source>(stream: &mut TokenStream<'source>) -> Result<Form<'source>, ParsingError<'source>> {
    let mut forms = Vec::new();

    let start_lexeme = if let Some(token) = stream.next() {
        if token.kind != TokenKind::OpenBrace {
            return Err(ParsingError::UnexpectedTokenExpected(token.clone(), TokenKind::OpenBrace));
        } else {
            token.lexeme
        }
    } else {
        return Err(ParsingError::UnexpectedEOF(None));
    };

    let mut end_lexeme = start_lexeme;

    loop {
        match stream.clone().next() {
            Some(token) => {
                if token.kind == TokenKind::CloseBrace {
                    stream.next();
                    break;
                }

                match parse_tokens(stream) {
                    Ok(Some(form)) => {
                        end_lexeme = form.lexeme;
                        let key = form;

                        match parse_tokens(stream) {
                            Ok(Some(form)) => {
                                end_lexeme = form.lexeme;
                                let value = form;
                                forms.push((key, value));
                            }
                            Ok(None) => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
                            Err(e) => return Err(e),
                        }
                    }
                    Ok(None) => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
                    Err(e) => return Err(e),
                }
            },
            None => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
        }
    }

    Ok(Form {
        kind: FormKind::Map(forms),
        lexeme: start_lexeme.join(&end_lexeme),
    })
}


pub fn parse_set<'source>(stream: &mut TokenStream<'source>) -> Result<Form<'source>, ParsingError<'source>> {
    let mut forms = Vec::new();

    let start_lexeme = if let Some(token) = stream.next() {
        if token.kind != TokenKind::OpenHashBrace {
            return Err(ParsingError::UnexpectedTokenExpected(token.clone(), TokenKind::OpenHashBrace));
        } else {
            token.lexeme
        }
    } else {
        return Err(ParsingError::UnexpectedEOF(None));
    };

    let mut end_lexeme = start_lexeme;

    loop {
        match stream.clone().next() {
            Some(token) => {
                if token.kind == TokenKind::CloseBrace {
                    stream.next();
                    break;
                }

                match parse_tokens(stream) {
                    Ok(Some(form)) => {
                        end_lexeme = form.lexeme;
                        forms.push(form);
                    }
                    Ok(None) => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
                    Err(e) => return Err(e),
                }
            },
            None => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
        }
    }

    Ok(Form {
        kind: FormKind::Vector(forms),
        lexeme: start_lexeme.join(&end_lexeme),
    })
}

impl Display for ParsingError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::UnexpectedEOF(lexeme) => if let Some(lexeme) = lexeme {
                write!(f, "Unexpected EOF at [{}:{}]", lexeme.line(), lexeme.column())
            } else {
                write!(f, "Unexpected EOF")
            },
            ParsingError::UnexpectedToken(token) => write!(f, "Unexpected token {}", token.kind),
            ParsingError::UnexpectedTokenExpected(token, expected) => write!(f, "Unexpected token [{}:{}] {}, expected {}", token.lexeme.line(), token.lexeme.column(), token.kind, expected),
            ParsingError::NonTerminalToken(token) => write!(f, "Non-terminal token [{}:{}] {}", token.lexeme.line(), token.lexeme.column(), token.kind),
        }
    }
}

impl Error for ParsingError<'_> {}

impl<'source> Display for Form<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            FormKind::List(forms) => {
                write!(f, "( ")?;
                for form in forms {
                    write!(f, "{} ", form)?;
                }
                write!(f, ")")
            },
            FormKind::Vector(forms) => {
                write!(f, "[ ")?;
                for form in forms {
                    write!(f, "{} ", form)?;
                }
                write!(f, "]")
            },
            FormKind::Map(forms) => {
                write!(f, "{{ ")?;
                for (key, value) in forms {
                    write!(f, "{} {}, ", key, value)?;
                }
                write!(f, "}}")
            },
            FormKind::Set(forms) => {
                write!(f, "#{{ ")?;
                for form in forms {
                    write!(f, "{} ", form)?;
                }
                write!(f, "}}")
            },
            FormKind::Keyword(span) => write!(f, ":{}", span),
            FormKind::Symbol(span) => write!(f, "{}", span),
            FormKind::Tag(span) => write!(f, "#{}", span),
            FormKind::Discard(span) => write!(f, "#_{}", span),
            FormKind::Integer(span) => write!(f, "{}", span),
            FormKind::Float(span) => write!(f, "{}", span),
            FormKind::Boolean(span) => write!(f, "{}", span),
            FormKind::Character(span) => write!(f, "\\{}", match span {
                '\n' => "newline",
                '\r' => "return",
                '\t' => "tab",
                ' ' => "space",
                c => return write!(f, "\\{}", c),
            }),
            FormKind::String(span) => write!(f, "\"{}\"", span),
            FormKind::Nil => write!(f, "nil"),
        }
    }
}