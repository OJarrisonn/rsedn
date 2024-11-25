#[cfg(test)]
mod tests {
    use std::collections::LinkedList;

    use rsedn::{
        lex_source, parse_lexeme,
        parser::{form::FormKind, parse_form},
        source_from_str,
    };

    #[test]
    fn tagged() {
        let src = "#my/tag (1 2 3)";
        let mut source = source_from_str(src);
        let lexemes = lex_source(&mut source);
        let tokens = lexemes
            .into_iter()
            .map(|lexeme| parse_lexeme(&source, lexeme))
            .map(|res| {
                assert!(res.is_ok());
                res.unwrap()
            })
            .collect::<LinkedList<_>>();
        let form = parse_form(&mut tokens.iter());
        assert!(form.is_ok());
        let form = form.unwrap();
        assert!(form.is_some());
        let form = form.unwrap();
        assert!(matches!(form.kind, FormKind::Tagged(_, _)));
        dbg!(form);
    }

    #[test]
    fn builtin_tag() {
        let src = "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"";
        let mut source = source_from_str(src);
        let lexemes = dbg!(lex_source(&mut source));
        let tokens = lexemes
            .into_iter()
            .map(|lexeme| parse_lexeme(&source, lexeme))
            .map(|res| {
                assert!(res.is_ok());
                res.unwrap()
            })
            .collect::<LinkedList<_>>();
        let form = parse_form(&mut tokens.iter());
        assert!(form.is_ok());
        let form = form.unwrap();
        assert!(form.is_some());
        let form = form.unwrap();
        assert!(matches!(form.kind, FormKind::UUID(_)));
        dbg!(form);
    }
}
