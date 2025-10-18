#[cfg(test)]
mod parser_tests {

    use tinyc::lexer::lexer::Lexer;
    use tinyc::lexer::token::Token;
    use tinyc::parser::parser::Parser;
    use tinyc::parser::productions::{Decls, Type};

    // Helper function to create a parser from a source string
    fn setup_parser(input: &str) -> (Parser, Vec<Token>) {
        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer.collect();
        let parser = Parser::new(tokens.clone());
        (parser, tokens)
    }
    #[test]
    fn test_var_decl_no_initialization() {
        let input = "int x;";
        let (mut parser, _tokens) = setup_parser(input);
        let result = parser.parse_decl();

        assert!(result.is_ok());
        match result.unwrap() {
            Decls::Var(var_decl) => {
                assert_eq!(var_decl.var_type, Type::Int);
                assert_eq!(var_decl.identifier.lexeme, "x");
                assert!(var_decl.initializer.is_none());
            }
            _ => panic!("Expected a variable declaration"),
        }
    }

    #[test]
    fn test_func_decl_no_params() {
        let input = "void main();";
        let (mut parser, _tokens) = setup_parser(input);
        let result = parser.parse_decl();

        assert!(result.is_ok());
        match result.unwrap() {
            Decls::Func(func_decl) => {
                assert_eq!(func_decl.return_type, Type::Void);
                assert_eq!(func_decl.identifier.lexeme, "main");
                assert!(func_decl.params.is_empty());
                assert!(func_decl.body.is_none());
            }
            _ => panic!("Expected a function declaration"),
        }
    }

    #[test]
    fn test_func_decl_with_params() {
        let input = "int add(int a, float b);";
        let (mut parser, _tokens) = setup_parser(input);
        let result = parser.parse_decl();

        assert!(result.is_ok());
        match result.unwrap() {
            Decls::Func(func_decl) => {
                assert_eq!(func_decl.return_type, Type::Int);
                assert_eq!(func_decl.identifier.lexeme, "add");
                assert_eq!(func_decl.params.len(), 2);

                assert_eq!(func_decl.params[0].param_type, Type::Int);
                assert_eq!(func_decl.params[0].identifier.lexeme, "a");

                assert_eq!(func_decl.params[1].param_type, Type::Float);
                assert_eq!(func_decl.params[1].identifier.lexeme, "b");

                assert!(func_decl.body.is_none());
            }
            _ => panic!("Expected a function declaration"),
        }
    }

    #[test]
    fn test_multiple_declarations() {
        let input = "int x; void foo(); float y;";
        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer.collect();
        let mut parser = Parser::new(tokens.clone());

        // First declaration: int x;
        let result1 = parser.parse_decl();
        assert!(result1.is_ok());
        match result1.unwrap() {
            Decls::Var(var_decl) => {
                assert_eq!(var_decl.var_type, Type::Int);
                assert_eq!(var_decl.identifier.lexeme, "x");
            }
            _ => panic!("Expected a variable declaration"),
        }

        // Second declaration: void foo();
        let result2 = parser.parse_decl();
        assert!(result2.is_ok());
        match result2.unwrap() {
            Decls::Func(func_decl) => {
                assert_eq!(func_decl.return_type, Type::Void);
                assert_eq!(func_decl.identifier.lexeme, "foo");
            }
            _ => panic!("Expected a function declaration"),
        }

        // Third declaration: float y;
        let result3 = parser.parse_decl();
        assert!(result3.is_ok());
        match result3.unwrap() {
            Decls::Var(var_decl) => {
                assert_eq!(var_decl.var_type, Type::Float);
                assert_eq!(var_decl.identifier.lexeme, "y");
            }
            _ => panic!("Expected a variable declaration"),
        }
    }
}
