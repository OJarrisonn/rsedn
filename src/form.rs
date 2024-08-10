use std::fmt::Display;

use crate::lexer::Lexeme;

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

impl<'source> Display for Form<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            FormKind::List(forms) => {
                write!(f, "( ")?;
                for form in forms {
                    write!(f, "{} ", form)?;
                }
                write!(f, ")")
            }
            FormKind::Vector(forms) => {
                write!(f, "[ ")?;
                for form in forms {
                    write!(f, "{} ", form)?;
                }
                write!(f, "]")
            }
            FormKind::Map(forms) => {
                write!(f, "{{ ")?;
                for (key, value) in forms {
                    write!(f, "{} {}, ", key, value)?;
                }
                write!(f, "}}")
            }
            FormKind::Set(forms) => {
                write!(f, "#{{ ")?;
                for form in forms {
                    write!(f, "{} ", form)?;
                }
                write!(f, "}}")
            }
            FormKind::Keyword(span) => write!(f, ":{}", span),
            FormKind::Symbol(span) => write!(f, "{}", span),
            FormKind::Tag(span) => write!(f, "#{}", span),
            FormKind::Discard(span) => write!(f, "#_{}", span),
            FormKind::Integer(span) => write!(f, "{}", span),
            FormKind::Float(span) => write!(f, "{}", span),
            FormKind::Boolean(span) => write!(f, "{}", span),
            FormKind::Character(span) => write!(
                f,
                "\\{}",
                match span {
                    '\n' => "newline",
                    '\r' => "return",
                    '\t' => "tab",
                    ' ' => "space",
                    c => return write!(f, "\\{}", c),
                }
            ),
            FormKind::String(span) => write!(f, "\"{}\"", span),
            FormKind::Nil => write!(f, "nil"),
        }
    }
}
