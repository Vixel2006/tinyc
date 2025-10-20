#[cfg(test)]
mod parser_tests {

    use tinyc::lexer::lexer::Lexer;
    use tinyc::lexer::token::{Token, TokenKind};
    use tinyc::parser::parser::Parser;
    use tinyc::parser::productions::{BinaryOp, Decls, Expr, Stmt, Type, UnaryOp, Val};

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

    #[test]
    fn test_variable_to_value_assignment_experssion() {
        let input = "int x = 0;";
        let (mut parser, _tokens) = setup_parser(input);

        let result = parser.parse_decl();
        assert!(result.is_ok());
        match result.unwrap() {
            Decls::Var(var_decl) => {
                assert_eq!(var_decl.var_type, Type::Int);
                assert_eq!(var_decl.identifier.lexeme, "x");
                assert!(var_decl.initializer.is_some());
                match var_decl.initializer.unwrap() {
                    Expr::Value(Val::Integer(val)) => assert_eq!(val, 0),
                    _ => panic!("Expected an integer initializer"),
                }
            }
            _ => panic!("Expected a variable initialization"),
        }
    }

    #[test]
    fn test_unary_and_paren_expressions() {
        let input = "int x = -(a + !b);";
        let (mut parser, _tokens) = setup_parser(input);

        let result = parser.parse_decl();
        assert!(result.is_ok());
        match result.unwrap() {
            Decls::Var(var_decl) => {
                assert_eq!(var_decl.var_type, Type::Int);
                assert_eq!(var_decl.identifier.lexeme, "x");
                assert!(var_decl.initializer.is_some());

                let initializer_expr = var_decl.initializer.unwrap();
                match initializer_expr {
                    Expr::Unary(op1, inner_expr1) => {
                        assert_eq!(op1, UnaryOp::Negate);
                        match *inner_expr1 {
                            Expr::Paren(inner_expr2) => match *inner_expr2 {
                                Expr::Binary(left, op2, right) => {
                                    assert_eq!(op2, BinaryOp::Add);
                                    match *left {
                                        Expr::Identifier(id_token_a) => {
                                            assert_eq!(id_token_a.lexeme, "a");
                                            assert_eq!(id_token_a.kind, TokenKind::Identifier);
                                        }
                                        _ => panic!("Expected identifier 'a'"),
                                    }
                                    match *right {
                                        Expr::Unary(op3, inner_expr3) => {
                                            assert_eq!(op3, UnaryOp::LogicalNot);
                                            match *inner_expr3 {
                                                Expr::Identifier(id_token_b) => {
                                                    assert_eq!(id_token_b.lexeme, "b");
                                                    assert_eq!(
                                                        id_token_b.kind,
                                                        TokenKind::Identifier
                                                    );
                                                }
                                                _ => panic!("Expected identifier 'b'"),
                                            }
                                        }
                                        _ => panic!("Expected unary logical NOT expression"),
                                    }
                                }
                                _ => panic!("Expected binary ADD expression"),
                            },
                            _ => panic!("Expected parenthesized expression"),
                        }
                    }
                    _ => panic!("Expected unary NEGATE expression"),
                }
            }
            _ => panic!("Expected a variable initialization"),
        }
    }

    #[test]
    fn test_binary_expression_precedence() {
        let input = "int x = a * b + c;";
        let (mut parser, _tokens) = setup_parser(input);

        let result = parser.parse_decl();
        assert!(result.is_ok());
        match result.unwrap() {
            Decls::Var(var_decl) => {
                assert_eq!(var_decl.var_type, Type::Int);
                assert_eq!(var_decl.identifier.lexeme, "x");
                assert!(var_decl.initializer.is_some());

                let initializer_expr = var_decl.initializer.unwrap();
                match initializer_expr {
                    Expr::Binary(left_add, op_add, right_add) => {
                        assert_eq!(op_add, BinaryOp::Add);
                        // Left side of ADD should be a * b
                        match *left_add {
                            Expr::Binary(left_mult, op_mult, right_mult) => {
                                assert_eq!(op_mult, BinaryOp::Mult);
                                match *left_mult {
                                    Expr::Identifier(id_token_a) => {
                                        assert_eq!(id_token_a.lexeme, "a");
                                        assert_eq!(id_token_a.kind, TokenKind::Identifier);
                                    }
                                    _ => panic!("Expected identifier 'a'"),
                                }
                                match *right_mult {
                                    Expr::Identifier(id_token_b) => {
                                        assert_eq!(id_token_b.lexeme, "b");
                                        assert_eq!(id_token_b.kind, TokenKind::Identifier);
                                    }
                                    _ => panic!("Expected identifier 'b'"),
                                }
                            }
                            _ => panic!("Expected binary MULT expression"),
                        }
                        // Right side of ADD should be c
                        match *right_add {
                            Expr::Identifier(id_token_c) => {
                                assert_eq!(id_token_c.lexeme, "c");
                                assert_eq!(id_token_c.kind, TokenKind::Identifier);
                            }
                            _ => panic!("Expected identifier 'c'"),
                        }
                    }
                    _ => panic!("Expected binary ADD expression as top-level"),
                }
            }
            _ => panic!("Expected a variable initialization"),
        }
    }

    #[test]
    fn test_binary_expression_precedence_with_literals() {
        let input = "int a = 10 * 2 + 3;";
        let (mut parser, _tokens) = setup_parser(input);

        let result = parser.parse_decl();
        assert!(result.is_ok());
        match result.unwrap() {
            Decls::Var(var_decl) => {
                assert_eq!(var_decl.var_type, Type::Int);
                assert_eq!(var_decl.identifier.lexeme, "a");
                assert!(var_decl.initializer.is_some());

                let initializer_expr = var_decl.initializer.unwrap();
                match initializer_expr {
                    Expr::Binary(left_add, op_add, right_add) => {
                        assert_eq!(op_add, BinaryOp::Add);
                        // Left side of ADD should be 10 * 2
                        match *left_add {
                            Expr::Binary(left_mult, op_mult, right_mult) => {
                                assert_eq!(op_mult, BinaryOp::Mult);
                                match *left_mult {
                                    Expr::Value(Val::Integer(val)) => assert_eq!(val, 10),
                                    _ => panic!("Expected integer literal 10"),
                                }
                                match *right_mult {
                                    Expr::Value(Val::Integer(val)) => assert_eq!(val, 2),
                                    _ => panic!("Expected integer literal 2"),
                                }
                            }
                            _ => panic!("Expected binary MULT expression"),
                        }
                        // Right side of ADD should be 3
                        match *right_add {
                            Expr::Value(Val::Integer(val)) => assert_eq!(val, 3),
                            _ => panic!("Expected integer literal 3"),
                        }
                    }
                    _ => panic!("Expected binary ADD expression as top-level"),
                }
            }
            _ => panic!("Expected a variable initialization"),
        }
    }

    #[test]
    fn test_if_statement() {
        let input = "if (true) { int x; }";
        let (mut parser, _tokens) = setup_parser(input);
        let result = parser.parse_stmt();

        assert!(result.is_ok());
        match result.unwrap() {
            Stmt::If(condition, body, else_branch) => {
                assert_eq!(condition, Expr::Value(Val::Bool(true)));
                assert_eq!(body.len(), 1);
                assert!(else_branch.is_none());

                match &body[0] {
                    Stmt::Decl(Decls::Var(var_decl)) => {
                        assert_eq!(var_decl.var_type, Type::Int);
                        assert_eq!(var_decl.identifier.lexeme, "x");
                    }
                    _ => panic!("Expected variable declaration in if body"),
                }
            }
            _ => panic!("Expected an if statement"),
        }
    }

    #[test]
    fn test_if_else_statement() {
        let input = "if (false) { } else { float y; }";
        let (mut parser, _tokens) = setup_parser(input);
        let result = parser.parse_stmt();

        assert!(result.is_ok());
        match result.unwrap() {
            Stmt::If(condition, body, else_branch) => {
                assert_eq!(condition, Expr::Value(Val::Bool(false)));
                assert!(body.is_empty());
                assert!(else_branch.is_some());

                let else_body = else_branch.unwrap();
                assert_eq!(else_body.len(), 1);
                match &else_body[0] {
                    Stmt::Decl(Decls::Var(var_decl)) => {
                        assert_eq!(var_decl.var_type, Type::Float);
                        assert_eq!(var_decl.identifier.lexeme, "y");
                    }
                    _ => panic!("Expected variable declaration in else body"),
                }
            }
            _ => panic!("Expected an if-else statement"),
        }
    }

    #[test]
    fn test_if_else_if_statement() {
        let input = "if (a) { } else if (b) { }";
        let (mut parser, _tokens) = setup_parser(input);
        let result = parser.parse_stmt();

        assert!(result.is_ok());
        match result.unwrap() {
            Stmt::If(condition, body, else_branch) => {
                assert_eq!(
                    condition,
                    Expr::Identifier(Token {
                        kind: TokenKind::Identifier,
                        lexeme: "a".to_string(),
                        line: 1,
                        column: 5,
                        length: 1,
                    })
                );
                assert!(body.is_empty());
                assert!(else_branch.is_some());

                let else_body = else_branch.unwrap();
                assert_eq!(else_body.len(), 1);
                match &else_body[0] {
                    Stmt::If(else_if_condition, else_if_body, else_if_else_branch) => {
                        assert_eq!(
                            *else_if_condition,
                            Expr::Identifier(Token {
                                kind: TokenKind::Identifier,
                                lexeme: "b".to_string(),
                                line: 1,
                                column: 21,
                                length: 1,
                            })
                        );
                        assert!(else_if_body.is_empty());
                        assert!(else_if_else_branch.is_none());
                    }
                    _ => panic!("Expected else-if statement"),
                }
            }
            _ => panic!("Expected an if-else-if statement"),
        }
    }

    #[test]
    fn test_while_statement() {
        let input = "while (x < 10) { x = x + 1; }";
        let (mut parser, _tokens) = setup_parser(input);
        let result = parser.parse_stmt();

        assert!(result.is_ok());
        match result.unwrap() {
            Stmt::While(condition, body) => {
                assert_eq!(
                    condition,
                    Expr::Binary(
                        Box::new(Expr::Identifier(Token {
                            kind: TokenKind::Identifier,
                            lexeme: "x".to_string(),
                            line: 1,
                            column: 8,
                            length: 1,
                        })),
                        BinaryOp::Less,
                        Box::new(Expr::Value(Val::Integer(10)))
                    )
                );
                assert_eq!(body.len(), 1);
                // Further assertions for the body can be added if needed
            }
            _ => panic!("Expected a while statement"),
        }
    }

    #[test]
    fn test_return_statement_with_expression() {
        let input = "return 0;";
        let (mut parser, _tokens) = setup_parser(input);
        let result = parser.parse_stmt();

        assert!(result.is_ok());
        match result.unwrap() {
            Stmt::Return(Some(expr)) => {
                assert_eq!(expr, Expr::Value(Val::Integer(0)));
            }
            _ => panic!("Expected a return statement with an expression"),
        }
    }

    #[test]
    fn test_return_statement_without_expression() {
        let input = "return;";
        let (mut parser, _tokens) = setup_parser(input);
        let result = parser.parse_stmt();

        assert!(result.is_ok());
        match result.unwrap() {
            Stmt::Return(None) => {}
            _ => panic!("Expected a return statement without an expression"),
        }
    }

    #[test]
    fn test_block_statement() {
        let input = "{ int a; float b = 1.0; }";
        let (mut parser, _tokens) = setup_parser(input);
        let result = parser.parse_stmt();

        assert!(result.is_ok());
        match result.unwrap() {
            Stmt::Block(statements) => {
                assert_eq!(statements.len(), 2);
                // Further assertions for the statements within the block can be added
            }
            _ => panic!("Expected a block statement"),
        }
    }

    #[test]
    fn test_full_program_parsing() {
        let input = "
            int main() {
                int x = 0;
                if (x == 0) {
                    return 1;
                } else {
                    while (x < 5) {
                        x = x + 1;
                    }
                    return 0;
                }
            }

            void foo(int a, float b);
            float global_var = 3.14;
        ";
        let (mut parser, _tokens) = setup_parser(input);
        let result = parser.parse_program();

        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.declarations.len(), 3); // main function, foo declaration, global_var

        match &program.declarations[0] {
            Stmt::Decl(Decls::Func(func_decl)) => {
                assert_eq!(func_decl.identifier.lexeme, "main");
                assert!(func_decl.body.is_some());
            }
            _ => panic!("Expected main function declaration"),
        }

        match &program.declarations[1] {
            Stmt::Decl(Decls::Func(func_decl)) => {
                assert_eq!(func_decl.identifier.lexeme, "foo");
                assert!(func_decl.body.is_none());
            }
            _ => panic!("Expected foo function declaration"),
        }

        match &program.declarations[2] {
            Stmt::Decl(Decls::Var(var_decl)) => {
                assert_eq!(var_decl.identifier.lexeme, "global_var");
                assert!(var_decl.initializer.is_some());
            }
            _ => panic!("Expected global_var declaration"),
        }
    }
}

