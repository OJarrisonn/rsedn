#[cfg(test)]
mod tests {
    use std::collections::LinkedList;

    use rsedn::{lexer::Source, parser::{parse_list, FormKind}, token::Token};

    #[test]
    fn simple_list() {
        let raw = "(+ 1 2)";
        let mut source: Source = raw.into();
        let lexemes = source.lex();
        let tokens = lexemes
            .into_iter()
            .map(|lexeme| Token::parse(&source, lexeme))
            .map(|token| token.unwrap())
            .collect::<LinkedList<_>>();
        let form = parse_list(&mut tokens.iter()).unwrap();

        assert!(matches!(form.kind, FormKind::List(_)));
    }
}