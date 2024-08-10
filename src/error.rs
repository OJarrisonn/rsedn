use crate::{token::TokenizationError, ParsingError};

pub enum Error<'source> {
    ParsingError(ParsingError<'source>),
    TokenizationError(TokenizationError),
}