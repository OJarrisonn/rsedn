#[cfg(test)]
mod tests {
    use std::collections::LinkedList;

    use rsedn::{form::FormKind, lex_source, parse_lexeme, parser::parse_form, source_from_str};

    #[test]
    fn tagged() {
        let src = "#my-tag (1 2 3)";
        let mut source = source_from_str(src);
        let lexemes = lex_source(&mut source);
        let tokens = lexemes
            .into_iter()
            .map(|lexeme| parse_lexeme(&source, lexeme))
            .map(|res| { assert!(res.is_ok()); res.unwrap() })
            .collect::<LinkedList<_>>();
        let form = parse_form(&mut tokens.iter());
        assert!(form.is_ok());
        let form = form.unwrap();
        assert!(matches!(form, Some(_)));
        let form = form.unwrap();
        assert!(matches!(form.kind, FormKind::Tag(_, _)));
        dbg!(form);
    }
}