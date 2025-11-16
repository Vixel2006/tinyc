use inkwell::values::{BasicValueEnum, PointerValue};

use super::context::CodeGenerator;
use crate::lexer::token::{Token, TokenKind};
use crate::parser::productions::{BinaryOp, Expr, UnaryOp};

impl<'ctx> Expr {
    pub fn codegen(&self, generator: &mut CodeGenerator<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            Expr::IntLiteral(value) => generator
                .context
                .i32_type()
                .const_int(*value as u64, false)
                .into(),
            Expr::Identifier(identifier) => self.codegen_identifier(identifier, generator).into(),
            Expr::Unary(op, expr) => self.codegen_unary(op, expr, generator),
            Expr::Binary(lhs, op, rhs) => self.codegen_binary(lhs, op, rhs, generator),
            Expr::Assignment(identifier, expr) => self.codegen_assignment(identifier, expr, generator),
            Expr::FunctionCall(fn_name, params) => self.codegen_function_call(fn_name, params, generator),
            _ => todo!(), // Handle other expression types
        }
    }

    fn codegen_identifier(
        &self,
        identifier: &Token,
        generator: &mut CodeGenerator<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let var_name = &identifier.lexeme;

        if let Some(alloca) = generator.variables.get(var_name) {
            generator
                .builder
                .build_load(alloca.to_owned(), var_name)
                .into()
        } else {
            eprintln!("Undeclared variable: {}", var_name);
            std::process::exit(1);
        }
    }

    fn codegen_unary(&self, _op: &UnaryOp, _expr: &Expr, _generator: &mut CodeGenerator<'ctx>) -> BasicValueEnum<'ctx> {
        // Here we need to define the unary operation and codegen the experation
        // we just start an instruction with the !, ~, -
        todo!()
    }

    fn codegen_binary(&self, _lhs: &Expr, _op: &BinaryOp, _rhs: &Expr, _generator: &mut CodeGenerator<'ctx>) -> BasicValueEnum<'ctx> {
        todo!()
    }

    fn codegen_assignment(&self, _identifier: &Token, _expr: &Expr, _generator: &mut CodeGenerator<'ctx>) -> BasicValueEnum<'ctx> {
        todo!()
    }

    fn codegen_function_call(
        &self,
        _fn_name: &Token,
        _params: &Vec<Expr>,
        _generator: &mut CodeGenerator<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}
