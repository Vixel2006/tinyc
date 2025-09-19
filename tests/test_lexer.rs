#[cfg(test)]
mod lexer_tests {
    use tinyc::lexer::lexer::Lexer;
    use tinyc::lexer::token::{Token, TokenKind};

    #[test]
    fn keyword_lexing() {
        let lexer = Lexer::new("if");
        let if_token: Option<Token> = Some(Token {
            kind: TokenKind::If,
            lexeme: "if".to_string(),
            length: 2,
            column: 1,
            line: 1,
        });

        let mut expected_tokens: Vec<Option<Token>> = Vec::new();

        for token in lexer {
            expected_tokens.push(Some(token));
        }
        assert_eq!(expected_tokens[0], if_token);
    }
}
