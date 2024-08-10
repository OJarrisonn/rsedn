#[cfg(test)]
mod tests {
    use std::collections::LinkedList;

    use rsedn::{parse_tokens, FormKind, Source, Token, TokenStream};


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
        let form = parse_tokens(&mut tokens.iter()).unwrap().unwrap();

        assert!(matches!(form.kind, FormKind::List(_)));
    }

    #[test]
    fn hello_world() {
        let mut source: Source = r#"
        (defn hello_world 
            "Documentation string" 
            [arg1 & args] 
            (println "Hello World " arg1 args))"#.into();
        let lexemes = source.lex();
        let tokens = lexemes
            .into_iter()
            .map(|lexeme| Token::parse(&source, lexeme))
            .map(|token| token.unwrap())
            .collect::<LinkedList<_>>();
        let form = parse_tokens(&mut tokens.iter()).unwrap().unwrap();

        assert!(matches!(form.kind, FormKind::List(_)));
    }
}