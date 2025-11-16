use inkwell::values::BasicValueEnum;
use inkwell::types::BasicTypeEnum;

use super::context::CodeGenerator;
use crate::lexer::token::Token;
use crate::parser::productions::{BinaryOp, Expr, UnaryOp, Val, Type};

impl<'ctx> Expr {
    pub fn codegen(&self, generator: &mut CodeGenerator<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            Expr::Value(val) => match val {
                Val::Integer(value) => generator
                    .context
                    .i32_type()
                    .const_int(*value as u64, false)
                    .into(),
                _ => todo!(), // Handle other literal types
            },
            Expr::Identifier(identifier) => self.codegen_identifier(identifier, generator),
            Expr::Unary(op, expr) => self.codegen_unary(op, expr, generator),
            Expr::Binary(lhs, op, rhs) => self.codegen_binary(lhs, op, rhs, generator),
            Expr::Assign(identifier, expr) => self.codegen_assignment(identifier, expr, generator),
            Expr::Call(fn_name, params) => self.codegen_function_call(fn_name, params, generator),
            _ => todo!(), // Handle other expression types like Paren
        }
    }

    fn codegen_identifier(
        &self,
        identifier: &Token,
        generator: &mut CodeGenerator<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let var_name = &identifier.lexeme;

        if let Some((alloca, var_type)) = generator.variables.get(var_name) {
            let llvm_type = generator.to_llvm_type(var_type);

            let loaded_value = match llvm_type {
                BasicTypeEnum::IntType(int_type) => generator
                    .builder
                    .build_load(int_type, *alloca, var_name)
                    .unwrap()
                    .into(),
                BasicTypeEnum::FloatType(float_type) => generator
                    .builder
                    .build_load(float_type, *alloca, var_name)
                    .unwrap()
                    .into(),
                _ => {
                    eprintln!("Unsupported LLVM type for variable: {}", var_name);
                    std::process::exit(1);
                }
            };
            loaded_value
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
