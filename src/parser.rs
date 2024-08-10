//! Takes tokens and builds an AST of Forms

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

impl PartialEq for Form<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

pub fn parse_tokens<'source>(
    stream: &mut TokenStream<'source>,
) -> Result<Option<Form<'source>>, String> {
    let token = match stream.clone().next() {
        Some(token) => token,
        None => return Ok(None),
    };

    match token.kind {
        TokenKind::OpenParen => parse_list(stream).map(Some),
        _ if token.is_terminal() => {stream.next(); parse_terminal_token(&token).map(Some)},
        _ => Err(format!(
            "Token ain't terminal: [{}:{}] {:?}",
            token.lexeme.line(),
            token.lexeme.column(),
            token
        )),
    }
}

/// Parse a terminal token into a Form
/// Returns an error if the token is not terminal
/// Won't consume the token, nor the token stream
pub fn parse_terminal_token<'source>(token: &Token<'source>) -> Result<Form<'source>, String> {
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
        _ => Err(format!(
            "Token ain't terminal: [{}:{}] {:?}",
            token.lexeme.line(),
            token.lexeme.column(),
            token
        )),
    };

    kind.map(|kind| Form {
        kind,
        lexeme: token.lexeme,
    })
}

pub fn parse_list<'source>(stream: &mut TokenStream<'source>) -> Result<Form<'source>, String> {
    let mut forms = Vec::new();

    let start_lexeme = if let Some(token) = stream.next() {
        if token.kind != TokenKind::OpenParen {
            return Err(format!(
                "Expected `(` at [{}:{}]",
                token.lexeme.line(),
                token.lexeme.column()
            ));
        } else {
            token.lexeme
        }
    } else {
        return Err("Unexpected EOF".to_string());
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
                    Ok(None) => return Err("Unexpected EOF".to_string()),
                    Err(e) => return Err(e),
                }
            },
            None => return Err("Unexpected EOF".to_string()),
        }
    }

    Ok(Form {
        kind: FormKind::List(forms),
        lexeme: start_lexeme.join(&end_lexeme),
    })
}

pub fn parse_vector<'source>(stream: &mut TokenStream<'source>) -> Result<Form<'source>, String> {
    let mut forms = Vec::new();

    let start_lexeme = if let Some(token) = stream.next() {
        if token.kind != TokenKind::OpenBracket {
            return Err(format!(
                "Expected `[` at [{}:{}]",
                token.lexeme.line(),
                token.lexeme.column()
            ));
        } else {
            token.lexeme
        }
    } else {
        return Err("Unexpected EOF".to_string());
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
                    Ok(None) => return Err("Unexpected EOF".to_string()),
                    Err(e) => return Err(e),
                }
            },
            None => return Err("Unexpected EOF".to_string()),
        }
    }

    Ok(Form {
        kind: FormKind::Vector(forms),
        lexeme: start_lexeme.join(&end_lexeme),
    })
}

