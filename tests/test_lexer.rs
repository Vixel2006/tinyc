#[cfg(test)]
mod lexer_tests {
    use tinyc::lexer::lexer::Lexer;
    use tinyc::lexer::token::TokenKind;

    #[test]
    fn keyword_lexing() {
        let lexer = Lexer::new("if else while int float bool char void return");
        let expected_tokens: Vec<TokenKind> = vec![
            TokenKind::If,
            TokenKind::Else,
            TokenKind::While,
            TokenKind::Int,
            TokenKind::Float,
            TokenKind::Bool,
            TokenKind::Char,
            TokenKind::Void,
            TokenKind::Return,
        ];

        let mut expected_idx = 0;

        for token in lexer {
            if token.kind != TokenKind::Whitespace && token.kind != TokenKind::Eof {
                assert_eq!(expected_tokens[expected_idx], token.kind);
                expected_idx += 1;
            }
        }
    }

    #[test]
    fn operator_lexing() {
        let lexer = Lexer::new("= == <= >= + - * / | & ~ ^ && || ! << >>");
        let expected_tokens: Vec<TokenKind> = vec![
            TokenKind::Eq,
            TokenKind::IsEq,
            TokenKind::LEq,
            TokenKind::GEq,
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Multiply,
            TokenKind::Div,
            TokenKind::BitwiseOr,
            TokenKind::BitwiseAnd,
            TokenKind::BitwiseNot,
            TokenKind::BitwiseXor,
            TokenKind::LogicalAnd,
            TokenKind::LogicalOr,
            TokenKind::LogicalNot,
            TokenKind::LeftShift,
            TokenKind::RightShift,
        ];

        let mut expected_idx = 0;

        for token in lexer {
            if token.kind != TokenKind::Whitespace && token.kind != TokenKind::Eof {
                assert_eq!(expected_tokens[expected_idx], token.kind);
                expected_idx += 1;
            }
        }
    }

    #[test]
    fn symbol_lexing() {
        let lexer = Lexer::new("( ) { } [ ]");
        let expected_tokens: Vec<TokenKind> = vec![
            TokenKind::LeftParen,
            TokenKind::RightParen,
            TokenKind::LeftCurly,
            TokenKind::RightCurly,
            TokenKind::LeftBracket,
            TokenKind::RightBracket,
        ];

        let mut expected_idx = 0;

        for token in lexer {
            if token.kind != TokenKind::Whitespace && token.kind != TokenKind::Eof {
                assert_eq!(expected_tokens[expected_idx], token.kind);
                expected_idx += 1;
            }
        }
    }

    #[test]
    fn punctuation_lexing() {
        let lexer = Lexer::new("; : . ,");
        let expected_tokens: Vec<TokenKind> = vec![
            TokenKind::SemiColon,
            TokenKind::Colon,
            TokenKind::Dot,
            TokenKind::Comma,
        ];

        let mut expected_idx = 0;

        for token in lexer {
            if token.kind != TokenKind::Whitespace && token.kind != TokenKind::Eof {
                assert_eq!(expected_tokens[expected_idx], token.kind);
                expected_idx += 1;
            }
        }
    }

    #[test]
    fn identifier_lexing() {
        let lexer = Lexer::new("ifx i n a whilex whil var_name");
        let expected_token: TokenKind = TokenKind::Identifier;

        for token in lexer {
            if token.kind != TokenKind::Whitespace && token.kind != TokenKind::Eof {
                assert_eq!(expected_token, token.kind);
            }
        }
    }

    #[test]
    fn whitespace_lexing() {
        let lexer = Lexer::new(" t\tt\n");
        let expected_tokens: Vec<TokenKind> = vec![
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Whitespace,
            TokenKind::Eof,
        ];

        let mut output_idx = 0;

        for token in lexer {
            assert_eq!(expected_tokens[output_idx], token.kind);
            output_idx += 1;
        }
    }

    #[test]
    fn new_line_increament() {
        let lexer = Lexer::new("\n\n\n");

        let mut line = 1;

        for token in lexer {
            assert_eq!(line, token.line);
            line += 1;
        }
    }

    #[test]
    fn token_length_calculation() {
        let lexer = Lexer::new("calculate the length of these words");

        for token in lexer {
            if token.kind != TokenKind::Whitespace && token.kind != TokenKind::Eof {
                assert_eq!(token.lexeme.len(), token.length);
            }
        }
    }

    #[test]
    fn column_increment() {
        let lexer = Lexer::new("this is a new sentence to test column increment");
        let mut column = 1;

        for token in lexer {
            if token.kind != TokenKind::Whitespace && token.kind != TokenKind::Eof {
                assert_eq!(column, token.column);
            }
            column += token.length as u32;
        }
    }

    #[test]
    fn full_code_snippet() {
        let lexer = Lexer::new("int main(void) {\nint i = 0;\nwhile (i < 10) {\ni += 1;\n}\n}\n");
        let expected_tokens: Vec<TokenKind> = vec![
            TokenKind::Int,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::LeftParen,
            TokenKind::Void,
            TokenKind::RightParen,
            TokenKind::Whitespace,
            TokenKind::LeftCurly,
            TokenKind::Whitespace,
            TokenKind::Int,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Whitespace,
            TokenKind::Eq,
            TokenKind::Whitespace,
            TokenKind::Unknown,
            TokenKind::SemiColon,
            TokenKind::Whitespace,
            TokenKind::While,
            TokenKind::Whitespace,
            TokenKind::LeftParen,
            TokenKind::Identifier,
            TokenKind::Whitespace,
            TokenKind::Less,
            TokenKind::Whitespace,
            TokenKind::Unknown,
            TokenKind::Unknown,
            TokenKind::RightParen,
            TokenKind::Whitespace,
            TokenKind::LeftCurly,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Whitespace,
            TokenKind::PlusEq,
            TokenKind::Whitespace,
            TokenKind::Unknown,
            TokenKind::SemiColon,
            TokenKind::Whitespace,
            TokenKind::RightCurly,
            TokenKind::Whitespace,
            TokenKind::RightCurly,
            TokenKind::Whitespace,
            TokenKind::Eof,
        ];

        let mut output_idx = 0;

        for token in lexer {
            assert_eq!(expected_tokens[output_idx], token.kind);
            output_idx += 1;
        }
    }
}
