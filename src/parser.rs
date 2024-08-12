//! Takes tokens and builds an AST of Forms

use std::{error::Error, fmt::Display};

use crate::{
    lexer::{
        lexeme::Lexeme,
        token::{Token, TokenKind, TokenStream},
    },
    parser::{
        builtin_tag::BuiltInTag,
        form::{Form, FormKind},
    },
};

pub mod builtin_tag;
pub mod form;

#[derive(Debug, Clone, PartialEq)]
pub enum ParsingError<'source> {
    UnexpectedEOF(Option<Lexeme>),
    UnexpectedToken(Token<'source>),
    UnexpectedTokenExpected(Token<'source>, TokenKind<'source>),
    NonTerminalToken(Token<'source>),
    UnexpectedForm(Form<'source>),
    UnexpectedSequence(String),
}

/// Take a token stream and parse it into a Form consuming tokens in the head of the stream
/// If the stream is empty, returns None
/// The token stream may not be empty after the call, so it's possible to continue parsing until it returns `Ok(None)`
pub fn parse_form<'source>(
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

        TokenKind::CloseBracket | TokenKind::CloseBrace | TokenKind::CloseParen => {
            Err(ParsingError::UnexpectedToken(token.clone()))
        }

        TokenKind::BuiltInTag(_) => parse_built_in_tagged_element(stream).map(Some),
        TokenKind::PrefixedTag(_) => parse_tagged_element(stream).map(Some),

        _ if token.is_terminal() => {
            stream.next();
            parse_terminal_token(&token).map(Some)
        }

        _ => Err(ParsingError::UnexpectedToken(token.clone())),
    }
}

/// Parse a terminal token into a Form
/// Returns an error if the token is not terminal
/// Won't consume the token, nor the token stream
fn parse_terminal_token<'source>(
    token: &Token<'source>,
) -> Result<Form<'source>, ParsingError<'source>> {
    let kind = match &token.kind {
        TokenKind::Keyword(span) => Ok(FormKind::Keyword(span)),
        TokenKind::Symbol(span) => Ok(FormKind::Symbol(span)),
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

fn parse_list<'source>(
    stream: &mut TokenStream<'source>,
) -> Result<Form<'source>, ParsingError<'source>> {
    let mut forms = Vec::new();

    let start_lexeme = if let Some(token) = stream.next() {
        if token.kind != TokenKind::OpenParen {
            return Err(ParsingError::UnexpectedTokenExpected(
                token.clone(),
                TokenKind::OpenParen,
            ));
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

                match parse_form(stream) {
                    Ok(Some(form)) => {
                        end_lexeme = form.lexeme;
                        forms.push(form);
                    }
                    Ok(None) => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
                    Err(e) => return Err(e),
                }
            }
            None => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
        }
    }

    Ok(Form {
        kind: FormKind::List(forms),
        lexeme: start_lexeme.join(&end_lexeme),
    })
}

fn parse_vector<'source>(
    stream: &mut TokenStream<'source>,
) -> Result<Form<'source>, ParsingError<'source>> {
    let mut forms = Vec::new();

    let start_lexeme = if let Some(token) = stream.next() {
        if token.kind != TokenKind::OpenBracket {
            return Err(ParsingError::UnexpectedTokenExpected(
                token.clone(),
                TokenKind::OpenBracket,
            ));
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

                match parse_form(stream) {
                    Ok(Some(form)) => {
                        end_lexeme = form.lexeme;
                        forms.push(form);
                    }
                    Ok(None) => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
                    Err(e) => return Err(e),
                }
            }
            None => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
        }
    }

    Ok(Form {
        kind: FormKind::Vector(forms),
        lexeme: start_lexeme.join(&end_lexeme),
    })
}

fn parse_map<'source>(
    stream: &mut TokenStream<'source>,
) -> Result<Form<'source>, ParsingError<'source>> {
    let mut forms = Vec::new();

    let start_lexeme = if let Some(token) = stream.next() {
        if token.kind != TokenKind::OpenBrace {
            return Err(ParsingError::UnexpectedTokenExpected(
                token.clone(),
                TokenKind::OpenBrace,
            ));
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

                match parse_form(stream) {
                    Ok(Some(form)) => {
                        end_lexeme = form.lexeme;
                        let key = form;

                        match parse_form(stream) {
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
            }
            None => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
        }
    }

    Ok(Form {
        kind: FormKind::Map(forms),
        lexeme: start_lexeme.join(&end_lexeme),
    })
}

fn parse_set<'source>(
    stream: &mut TokenStream<'source>,
) -> Result<Form<'source>, ParsingError<'source>> {
    let mut forms = Vec::new();

    let start_lexeme = if let Some(token) = stream.next() {
        if token.kind != TokenKind::OpenHashBrace {
            return Err(ParsingError::UnexpectedTokenExpected(
                token.clone(),
                TokenKind::OpenHashBrace,
            ));
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

                match parse_form(stream) {
                    Ok(Some(form)) => {
                        end_lexeme = form.lexeme;
                        forms.push(form);
                    }
                    Ok(None) => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
                    Err(e) => return Err(e),
                }
            }
            None => return Err(ParsingError::UnexpectedEOF(Some(end_lexeme))),
        }
    }

    Ok(Form {
        kind: FormKind::Vector(forms),
        lexeme: start_lexeme.join(&end_lexeme),
    })
}

fn parse_built_in_tagged_element<'source>(
    stream: &mut TokenStream<'source>,
) -> Result<Form<'source>, ParsingError<'source>> {
    // Take the tag
    let (tag, lexeme) = if let Some(token) = stream.next() {
        if let TokenKind::BuiltInTag(tag) = token.kind {
            (tag, token.lexeme)
        } else {
            return Err(ParsingError::UnexpectedToken(token.clone()));
        }
    } else {
        return Err(ParsingError::UnexpectedEOF(None));
    };

    // Parse the form
    let form = match parse_form(stream) {
        Ok(Some(form)) => form,
        Ok(None) => return Err(ParsingError::UnexpectedEOF(Some(lexeme))),
        Err(e) => return Err(e),
    };

    // Match the tag and the form
    match tag {
        BuiltInTag::Inst => {
            if let FormKind::String(string) = &form.kind {
                let dt = if let Ok(dt) = chrono::DateTime::parse_from_rfc3339(&string) {
                    dt.into()
                } else {
                    return Err(ParsingError::UnexpectedSequence(string.clone()));
                };

                Ok(Form {
                    kind: FormKind::Inst(dt),
                    lexeme: lexeme.join(&form.lexeme),
                })
            } else {
                Err(ParsingError::UnexpectedForm(form))
            }
        }
        BuiltInTag::UUID => {
            if let FormKind::String(string) = &form.kind {
                let uuid = if let Ok(uuid) = uuid::Uuid::parse_str(&string) {
                    uuid
                } else {
                    return Err(ParsingError::UnexpectedSequence(string.clone()));
                };

                Ok(Form {
                    kind: FormKind::UUID(uuid),
                    lexeme: lexeme.join(&form.lexeme),
                })
            } else {
                Err(ParsingError::UnexpectedForm(form))
            }
        }
    }
}

fn parse_tagged_element<'source>(
    stream: &mut TokenStream<'source>,
) -> Result<Form<'source>, ParsingError<'source>> {
    let (tag, lexeme) = if let Some(token) = stream.next() {
        if let TokenKind::PrefixedTag(tag) = token.kind {
            (tag, token.lexeme)
        } else {
            return Err(ParsingError::UnexpectedToken(token.clone()));
        }
    } else {
        return Err(ParsingError::UnexpectedEOF(None));
    };

    let form = match parse_form(stream) {
        Ok(Some(form)) => form,
        Ok(None) => return Err(ParsingError::UnexpectedEOF(Some(lexeme))),
        Err(e) => return Err(e),
    };

    Ok(Form {
        lexeme: lexeme.join(&form.lexeme),
        kind: FormKind::Tagged(tag, Box::new(form)),
    })
}

impl Display for ParsingError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::UnexpectedEOF(lexeme) => {
                if let Some(lexeme) = lexeme {
                    write!(
                        f,
                        "Unexpected EOF at [{}:{}]",
                        lexeme.line(),
                        lexeme.column()
                    )
                } else {
                    write!(f, "Unexpected EOF")
                }
            }
            ParsingError::UnexpectedToken(token) => write!(f, "Unexpected token {}", token.kind),
            ParsingError::UnexpectedTokenExpected(token, expected) => write!(
                f,
                "Unexpected token [{}:{}] {}, expected {}",
                token.lexeme.line(),
                token.lexeme.column(),
                token.kind,
                expected
            ),
            ParsingError::NonTerminalToken(token) => write!(
                f,
                "Non-terminal token [{}:{}] {}",
                token.lexeme.line(),
                token.lexeme.column(),
                token.kind
            ),
            ParsingError::UnexpectedForm(form) => write!(f, "Unexpected form {:?}", form.kind),
            ParsingError::UnexpectedSequence(seq) => write!(f, "Unexpected sequence {}", seq),
        }
    }
}

impl Error for ParsingError<'_> {}
