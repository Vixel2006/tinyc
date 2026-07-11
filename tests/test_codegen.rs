#[cfg(test)]
mod codegen_tests {

    use inkwell::context::Context;
    use tinyc::codegen::context::CodeGenerator;
    use tinyc::lexer::lexer::Lexer;
    use tinyc::parser::parser::Parser;

    fn build_generator<'ctx>(context: &'ctx Context, input: &str) -> CodeGenerator<'ctx> {
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();
        let mut generator = CodeGenerator::new(context, "test");
        for decl in &program.declarations {
            decl.codegen(&mut generator);
        }
        generator
    }

    fn assert_module_valid(input: &str) {
        let context = Context::create();
        let generator = build_generator(&context, input);
        assert!(
            generator.module.verify().is_ok(),
            "Module verification failed: {:?}",
            generator.module.verify()
        );
    }

    fn assert_ir_contains(input: &str, expected: &str) {
        let context = Context::create();
        let generator = build_generator(&context, input);
        let ir = generator.module.print_to_string().to_string();
        assert!(
            ir.contains(expected),
            "IR should contain '{}' but got:\n{}",
            expected,
            ir
        );
    }

    // ── Variable declarations inside functions ──

    #[test]
    fn test_var_decl_no_initializer() {
        assert_module_valid("void f() { int x; }");
    }

    #[test]
    fn test_var_decl_int_initializer() {
        assert_module_valid("void f() { int x = 42; }");
    }

    #[test]
    fn test_var_decl_float_initializer() {
        assert_module_valid("void f() { float x = 3.14; }");
    }

    #[test]
    fn test_var_decl_bool_initializer() {
        assert_module_valid("void f() { bool x = true; }");
    }

    #[test]
    fn test_var_decl_zero_false() {
        assert_module_valid("void f() { int x = 0; bool y = false; }");
    }

    // ── Function declarations ──

    #[test]
    fn test_func_decl_no_params_no_body() {
        assert_module_valid("void foo();");
    }

    #[test]
    fn test_func_with_body() {
        assert_module_valid("void foo() { int x; }");
    }

    #[test]
    fn test_func_with_params() {
        assert_module_valid("int add(int a, float b);");
    }

    #[test]
    fn test_func_implicit_return_int() {
        assert_module_valid("int f() { int x = 0; }");
    }

    #[test]
    fn test_func_implicit_return_float() {
        assert_module_valid("float f() { float x = 1.0; }");
    }

    #[test]
    fn test_func_implicit_return_bool() {
        assert_module_valid("bool f() { bool x = true; }");
    }

    #[test]
    fn test_multiple_functions() {
        assert_module_valid("void foo() { } void bar() { } int baz() { return 0; }");
    }

    // ── Return statements ──

    #[test]
    fn test_return_int() {
        assert_ir_contains("int f() { return 42; }", "ret i32 42");
        assert_module_valid("int f() { return 42; }");
    }

    #[test]
    fn test_return_void() {
        assert_ir_contains("void f() { return; }", "ret void");
        assert_module_valid("void f() { return; }");
    }

    #[test]
    fn test_return_float() {
        assert_module_valid("float f() { return 3.14; }");
    }

    #[test]
    fn test_return_bool_true() {
        assert_module_valid("bool f() { return true; }");
    }

    #[test]
    fn test_return_bool_false() {
        assert_module_valid("bool f() { return false; }");
    }

    // ── Integer arithmetic ──

    #[test]
    fn test_int_add() {
        assert_module_valid("int f() { return 1 + 2; }");
    }

    #[test]
    fn test_int_sub() {
        assert_module_valid("int f() { return 5 - 3; }");
    }

    #[test]
    fn test_int_mult() {
        assert_module_valid("int f() { return 2 * 3; }");
    }

    #[test]
    fn test_int_div() {
        assert_module_valid("int f() { return 10 / 2; }");
    }

    #[test]
    fn test_complex_arithmetic() {
        assert_module_valid("int f() { return (1 + 2) * (3 - 4) / 5; }");
    }

    // ── Float arithmetic ──

    #[test]
    fn test_float_add() {
        assert_module_valid("float f() { return 1.5 + 2.5; }");
    }

    #[test]
    fn test_float_sub() {
        assert_module_valid("float f() { return 5.5 - 2.0; }");
    }

    #[test]
    fn test_float_mult() {
        assert_module_valid("float f() { return 2.0 * 3.0; }");
    }

    #[test]
    fn test_float_div() {
        assert_module_valid("float f() { return 10.0 / 2.0; }");
    }

    // ── Unary operators ──

    #[test]
    fn test_negate_int() {
        assert_module_valid("int f() { return -(5); }");
    }

    #[test]
    fn test_negate_float() {
        assert_module_valid("float f() { return -(3.14); }");
    }

    #[test]
    fn test_logical_not() {
        assert_module_valid("bool f() { return !true; }");
    }

    #[test]
    fn test_bitwise_not() {
        assert_module_valid("int f() { return ~5; }");
    }

    // ── Comparisons (return bool) ──

    #[test]
    fn test_less_than() {
        assert_module_valid("bool f() { return 1 < 2; }");
    }

    #[test]
    fn test_less_equal() {
        assert_module_valid("bool f() { return 1 <= 2; }");
    }

    #[test]
    fn test_greater_than() {
        assert_module_valid("bool f() { return 2 > 1; }");
    }

    #[test]
    fn test_greater_equal() {
        assert_module_valid("bool f() { return 2 >= 1; }");
    }

    #[test]
    fn test_equal() {
        assert_module_valid("bool f() { return 1 == 2; }");
    }

    #[test]
    fn test_not_equal() {
        assert_module_valid("bool f() { return 1 != 2; }");
    }

    #[test]
    fn test_float_less_than() {
        assert_module_valid("bool f() { return 1.5 < 2.5; }");
    }

    #[test]
    fn test_float_equal() {
        assert_module_valid("bool f() { return 1.5 == 1.5; }");
    }

    // ── Variables and assignment ──

    #[test]
    fn test_variable_read() {
        assert_module_valid("int f() { int x = 1; return x; }");
    }

    #[test]
    fn test_assignment() {
        assert_module_valid("int f() { int x; x = 5; return x; }");
    }

    #[test]
    fn test_var_read_float() {
        assert_module_valid("float f() { float x = 1.5; return x; }");
    }

    // ── Control flow ──

    #[test]
    fn test_if_stmt() {
        assert_module_valid("int f() { if (1) { return 0; } return 1; }");
    }

    #[test]
    fn test_if_else_stmt() {
        assert_module_valid("int f() { if (0) { return 1; } else { return 2; } }");
    }

    #[test]
    fn test_while_stmt() {
        assert_module_valid("int f() { int x = 0; while (x < 3) { x = x + 1; } return x; }");
    }

    #[test]
    fn test_block_stmt() {
        assert_module_valid("int f() { { int x; int y; } return 0; }");
    }

    #[test]
    fn test_expr_as_stmt() {
        assert_module_valid("int f() { 1 + 2; return 0; }");
    }

    #[test]
    fn test_if_else_if_chain() {
        assert_module_valid(
            "int f() { if (1) { return 0; } else if (2) { return 1; } else { return 2; } }",
        );
    }

    // ── Multi-param functions ──

    #[test]
    fn test_two_int_params() {
        assert_module_valid("int add(int a, int b) { return a + b; }");
    }

    #[test]
    fn test_mixed_params() {
        // Uses matching types for arithmetic; the int param is stored but not used in float op
        assert_module_valid("float add(int a, float b) { float x = b; return x; }");
    }

    // ── Parenthesized expressions ──

    #[test]
    fn test_paren_expr() {
        assert_module_valid("int f() { return (1 + 2) * 3; }");
    }

    // ── to_llvm_type ──

    #[test]
    fn test_to_llvm_type_int() {
        let context = Context::create();
        let generator = CodeGenerator::new(&context, "test");
        assert!(generator.to_llvm_type(&tinyc::parser::productions::Type::Int).is_int_type());
    }

    #[test]
    fn test_to_llvm_type_float() {
        let context = Context::create();
        let generator = CodeGenerator::new(&context, "test");
        assert!(generator.to_llvm_type(&tinyc::parser::productions::Type::Float).is_float_type());
    }

    #[test]
    fn test_to_llvm_type_bool() {
        let context = Context::create();
        let generator = CodeGenerator::new(&context, "test");
        assert!(generator.to_llvm_type(&tinyc::parser::productions::Type::Bool).is_int_type());
    }

    #[test]
    fn test_to_llvm_type_char() {
        let context = Context::create();
        let generator = CodeGenerator::new(&context, "test");
        assert!(generator.to_llvm_type(&tinyc::parser::productions::Type::Char).is_int_type());
    }

    // ── Full program ──

    #[test]
    fn test_full_program() {
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
        ";
        let context = Context::create();
        let generator = build_generator(&context, input);
        let ir = generator.module.print_to_string().to_string();
        assert!(ir.contains("main"));
        assert!(ir.contains("foo"));
        assert!(generator.module.verify().is_ok());
    }
}
