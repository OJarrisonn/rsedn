#[cfg(test)]
mod tests {

    #[test]
    fn main_test() {
        use std::collections::LinkedList;

        use rsedn::{
            lexer::{source::Source, token::Token},
            parser::{self, form::FormKind},
        };
        // A Source can be created from a &str
        let mut source: Source = "(defn add [a b] (+ a b))".into();
        // Lex the source into Vec<Lexeme>
        let lexemes = source.lex();
        // Parse the lexemes into a LinkedList<Token>
        let tokens = lexemes
            .into_iter()
            .map(|lexeme| Token::parse(&source, lexeme)) // Parse the lexeme into a Token
            .map(|token| token.unwrap()) // Unwrap the Result<Token, ParsingError>
            .collect::<LinkedList<_>>();
        let mut token_stream = tokens.iter(); // Create a TokenStream from the LinkedList
        let form = parser::parse_form(&mut token_stream).unwrap().unwrap(); // Parse the tokens into a Form

        assert!(matches!(form.kind, FormKind::List(_)));
    }
}
