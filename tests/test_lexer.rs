#[cfg(test)]
mod lexer_tests {
    use tinyc::lexer::lexer::Lexer;
    use tinyc::lexer::token::TokenKind;

    #[test]
    fn keyword_lexing() {
        let lexer = Lexer::new("if else while int float bool char void return true false");
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
            TokenKind::True,
            TokenKind::False,
            TokenKind::Eof, // Lexer now always ends with Eof
        ];

        let mut expected_idx = 0;

        for token in lexer {
            assert_eq!(expected_tokens[expected_idx], token.kind);
            expected_idx += 1;
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
            TokenKind::Eof, // Lexer now always ends with Eof
        ];

        let mut expected_idx = 0;

        for token in lexer {
            assert_eq!(expected_tokens[expected_idx], token.kind);
            expected_idx += 1;
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
            TokenKind::Eof, // Lexer now always ends with Eof
        ];

        let mut expected_idx = 0;

        for token in lexer {
            assert_eq!(expected_tokens[expected_idx], token.kind);
            expected_idx += 1;
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
            TokenKind::Eof, // Lexer now always ends with Eof
        ];

        let mut expected_idx = 0;

        for token in lexer {
            assert_eq!(expected_tokens[expected_idx], token.kind);
            expected_idx += 1;
        }
    }

    #[test]
    fn identifier_lexing() {
        let lexer = Lexer::new("ifx i n a whilex whil var_name");
        let expected_kinds: Vec<TokenKind> = vec![
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Eof,
        ];
        let mut expected_idx = 0;

        for token in lexer {
            assert_eq!(expected_kinds[expected_idx], token.kind);
            expected_idx += 1;
        }
    }

    // This test is no longer relevant as the lexer skips whitespace.
    // #[test]
    // fn whitespace_lexing() {
    //     let lexer = Lexer::new(" t\tt\n");
    //     let expected_tokens: Vec<TokenKind> = vec![
    //         TokenKind::Whitespace,
    //         TokenKind::Identifier,
    //         TokenKind::Whitespace,
    //         TokenKind::Identifier,
    //         TokenKind::Whitespace,
    //         TokenKind::Eof,
    //     ];

    //     let mut output_idx = 0;

    //     for token in lexer {
    //         assert_eq!(expected_tokens[output_idx], token.kind);
    //         output_idx += 1;
    //     }
    // }

    #[test]
    fn char_literal_lexing() {
        let lexer = Lexer::new("'t' 'x' '\t' '\n' ' ' ''");
        let expected_tokens: Vec<TokenKind> = vec![
            TokenKind::Character,
            TokenKind::Character,
            TokenKind::Character,
            TokenKind::Character,
            TokenKind::Character,
            TokenKind::Unknown, // '' is an unknown token
            TokenKind::Eof,
        ];

        let mut output_idx = 0;

        for token in lexer {
            assert_eq!(expected_tokens[output_idx], token.kind);
            output_idx += 1;
        }
    }

    #[test]
    fn new_line_increment() {
        let lexer = Lexer::new("first\nsecond\nthird");
        let mut tokens = lexer.collect::<Vec<_>>();
        // The lexer now skips newlines, but it still tracks line numbers.
        // The Eof token should reflect the line number after all input has been processed.
        // "first" (line 1), "second" (line 2), "third" (line 3), Eof (line 3)
        assert_eq!(tokens[0].line, 1); // "first"
        assert_eq!(tokens[1].line, 2); // "second"
        assert_eq!(tokens[2].line, 3); // "third"
        assert_eq!(tokens[3].kind, TokenKind::Eof);
        assert_eq!(tokens[3].line, 3); // Eof should be on the last line of content
    }

    #[test]
    fn token_length_calculation() {
        let lexer = Lexer::new("calculate the length of these words");

        for token in lexer {
            if token.kind != TokenKind::Eof {
                // No need to check for Whitespace anymore
                assert_eq!(token.lexeme.len(), token.length);
            }
        }
    }

    #[test]
    fn column_increment() {
        let lexer = Lexer::new("this is a new sentence to test column increment");
        let expected_columns: Vec<u32> = vec![
            1,  // this
            6,  // is
            9,  // a
            11, // new
            15, // sentence
            24, // to
            27, // test
            32, // column
            39, // increment
            48, // Eof
        ];
        let mut expected_idx = 0;

        for token in lexer {
            assert_eq!(expected_columns[expected_idx], token.column);
            expected_idx += 1;
        }
    }

    #[test]
    fn full_code_snippet() {
        let lexer = Lexer::new("int main(void) {\nint i = 0;\nwhile (i < 10.5) {\ni += 1;\n}\n}\n");
        let expected_tokens: Vec<TokenKind> = vec![
            TokenKind::Int,
            TokenKind::Identifier,
            TokenKind::LeftParen,
            TokenKind::Void,
            TokenKind::RightParen,
            TokenKind::LeftCurly,
            TokenKind::Int,
            TokenKind::Identifier,
            TokenKind::Eq,
            TokenKind::Integer,
            TokenKind::SemiColon,
            TokenKind::While,
            TokenKind::LeftParen,
            TokenKind::Identifier,
            TokenKind::Less,
            TokenKind::Decimal,
            TokenKind::RightParen,
            TokenKind::LeftCurly,
            TokenKind::Identifier,
            TokenKind::PlusEq,
            TokenKind::Integer,
            TokenKind::SemiColon,
            TokenKind::RightCurly,
            TokenKind::RightCurly,
            TokenKind::Eof,
        ];

        let mut output_idx = 0;

        for token in lexer {
            assert_eq!(expected_tokens[output_idx], token.kind);
            output_idx += 1;
        }
    }
}
