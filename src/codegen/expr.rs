use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::BasicValueEnum;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;

use super::context::CodeGenerator;
use crate::lexer::token::Token;
use crate::parser::productions::{BinaryOp, Expr, UnaryOp, Val};

impl<'ctx> Expr {
    pub fn codegen(&self, generator: &mut CodeGenerator<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            Expr::Value(val) => match val {
                Val::Integer(value) => generator
                    .context
                    .i32_type()
                    .const_int(*value as u64, false)
                    .into(),
                Val::Float(value) => generator
                    .context
                    .f64_type()
                    .const_float(*value as f64)
                    .into(),
                Val::Bool(value) => generator
                    .context
                    .bool_type()
                    .const_int(if *value { 1 } else { 0 }, false)
                    .into(),
                Val::Char(value) => generator
                    .context
                    .i8_type()
                    .const_int(*value as u64, false)
                    .into(),
                Val::String(value) => {
                    let str_val = generator
                        .builder
                        .build_global_string_ptr(value, "str")
                        .unwrap()
                        .as_pointer_value();
                    generator
                        .builder
                        .build_load(generator.context.i8_type(), str_val, "strtmp")
                        .unwrap()
                }
            },
            Expr::Identifier(identifier) => self.codegen_identifier(identifier, generator),
            Expr::Unary(op, expr) => self.codegen_unary(op, expr, generator),
            Expr::Binary(lhs, op, rhs) => self.codegen_binary(lhs, op, rhs, generator),
            Expr::Assign(identifier, expr) => self.codegen_assignment(identifier, expr, generator),
            Expr::Call(fn_name, params) => self.codegen_function_call(fn_name, params, generator),
            Expr::Paren(expr) => expr.codegen(generator),
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

    fn codegen_unary(
        &self,
        op: &UnaryOp,
        expr: &Expr,
        generator: &mut CodeGenerator<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let val = expr.codegen(generator);
        match op {
            UnaryOp::Negate => match val {
                BasicValueEnum::IntValue(int_val) => generator
                    .builder
                    .build_int_neg(int_val, "negtmp")
                    .unwrap()
                    .into(),
                BasicValueEnum::FloatValue(float_val) => generator
                    .builder
                    .build_float_neg(float_val, "negtmp")
                    .unwrap()
                    .into(),
                _ => todo!("Negate not supported for this type"),
            },
            UnaryOp::LogicalNot => {
                let truthy = generator.build_truthy(val, "booltmp");
                generator
                    .builder
                    .build_not(truthy, "nottmp")
                    .unwrap()
                    .into()
            }
            UnaryOp::BitwiseNot => match val {
                BasicValueEnum::IntValue(int_val) => generator
                    .builder
                    .build_not(int_val, "nottmp")
                    .unwrap()
                    .into(),
                _ => todo!("Bitwise not not supported for this type"),
            },
        }
    }

    fn codegen_binary(
        &self,
        lhs: &Expr,
        op: &BinaryOp,
        rhs: &Expr,
        generator: &mut CodeGenerator<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let lhs_val = lhs.codegen(generator);
        let rhs_val = rhs.codegen(generator);

        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mult | BinaryOp::Div => {
                match (&lhs_val, &rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => match op {
                        BinaryOp::Add => generator
                            .builder
                            .build_int_add(*l, *r, "addtmp")
                            .unwrap()
                            .into(),
                        BinaryOp::Sub => generator
                            .builder
                            .build_int_sub(*l, *r, "subtmp")
                            .unwrap()
                            .into(),
                        BinaryOp::Mult => generator
                            .builder
                            .build_int_mul(*l, *r, "multmp")
                            .unwrap()
                            .into(),
                        BinaryOp::Div => generator
                            .builder
                            .build_int_signed_div(*l, *r, "divtmp")
                            .unwrap()
                            .into(),
                        _ => unreachable!(),
                    },
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                        match op {
                            BinaryOp::Add => generator
                                .builder
                                .build_float_add(*l, *r, "addtmp")
                                .unwrap()
                                .into(),
                            BinaryOp::Sub => generator
                                .builder
                                .build_float_sub(*l, *r, "subtmp")
                                .unwrap()
                                .into(),
                            BinaryOp::Mult => generator
                                .builder
                                .build_float_mul(*l, *r, "multtmp")
                                .unwrap()
                                .into(),
                            BinaryOp::Div => generator
                                .builder
                                .build_float_div(*l, *r, "divtmp")
                                .unwrap()
                                .into(),
                            _ => unreachable!(),
                        }
                    }
                    _ => todo!("Mixed type arithmetic not supported yet"),
                }
            }
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Greater
            | BinaryOp::Less
            | BinaryOp::GreaterEqual
            | BinaryOp::LessEqual => {
                let cmp: BasicValueEnum = match (&lhs_val, &rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                        let pred = match op {
                            BinaryOp::Equal => IntPredicate::EQ,
                            BinaryOp::NotEqual => IntPredicate::NE,
                            BinaryOp::Greater => IntPredicate::SGT,
                            BinaryOp::Less => IntPredicate::SLT,
                            BinaryOp::GreaterEqual => IntPredicate::SGE,
                            BinaryOp::LessEqual => IntPredicate::SLE,
                            _ => unreachable!(),
                        };
                        generator
                            .builder
                            .build_int_compare(pred, *l, *r, "cmptmp")
                            .unwrap()
                            .into()
                    }
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                        let pred = match op {
                            BinaryOp::Equal => FloatPredicate::OEQ,
                            BinaryOp::NotEqual => FloatPredicate::ONE,
                            BinaryOp::Greater => FloatPredicate::OGT,
                            BinaryOp::Less => FloatPredicate::OLT,
                            BinaryOp::GreaterEqual => FloatPredicate::OGE,
                            BinaryOp::LessEqual => FloatPredicate::OLE,
                            _ => unreachable!(),
                        };
                        generator
                            .builder
                            .build_float_compare(pred, *l, *r, "cmptmp")
                            .unwrap()
                            .into()
                    }
                    _ => todo!("Mixed type comparison not supported yet"),
                };
                cmp
            }
            BinaryOp::LogicalAnd => {
                let lhs_i1 = generator.build_truthy(lhs_val, "booltmp");
                let rhs_i1 = generator.build_truthy(rhs_val, "booltmp");
                generator
                    .builder
                    .build_and(lhs_i1, rhs_i1, "andtmp")
                    .unwrap()
                    .into()
            }
            BinaryOp::LogicalOr => {
                let lhs_i1 = generator.build_truthy(lhs_val, "booltmp");
                let rhs_i1 = generator.build_truthy(rhs_val, "booltmp");
                generator
                    .builder
                    .build_or(lhs_i1, rhs_i1, "ortmp")
                    .unwrap()
                    .into()
            }
            BinaryOp::BitwiseAnd => match (&lhs_val, &rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => generator
                    .builder
                    .build_and(*l, *r, "andtmp")
                    .unwrap()
                    .into(),
                _ => todo!("Bitwise AND not supported for this type"),
            },
            BinaryOp::BitwiseOr => match (&lhs_val, &rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => generator
                    .builder
                    .build_or(*l, *r, "ortmp")
                    .unwrap()
                    .into(),
                _ => todo!("Bitwise OR not supported for this type"),
            },
            BinaryOp::BitwiseXor => match (&lhs_val, &rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => generator
                    .builder
                    .build_xor(*l, *r, "xortmp")
                    .unwrap()
                    .into(),
                _ => todo!("Bitwise XOR not supported for this type"),
            },
            BinaryOp::LeftShift => match (&lhs_val, &rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => generator
                    .builder
                    .build_left_shift(*l, *r, "shltmp")
                    .unwrap()
                    .into(),
                _ => todo!("Left shift not supported for this type"),
            },
            BinaryOp::RightShift => match (&lhs_val, &rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => generator
                    .builder
                    .build_right_shift(*l, *r, true, "shrtmp")
                    .unwrap()
                    .into(),
                _ => todo!("Right shift not supported for this type"),
            },
        }
    }

    fn codegen_assignment(
        &self,
        identifier: &Token,
        expr: &Expr,
        generator: &mut CodeGenerator<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let val = expr.codegen(generator);
        let var_name = &identifier.lexeme;

        if let Some((alloca, _)) = generator.variables.get(var_name) {
            generator.builder.build_store(*alloca, val).unwrap();
            val
        } else {
            panic!("Undeclared variable: {}", var_name);
        }
    }

    fn codegen_function_call(
        &self,
        fn_name: &Token,
        params: &Vec<Expr>,
        generator: &mut CodeGenerator<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let fn_val = match generator.functions.get(&fn_name.lexeme) {
            Some((fn_val, _)) => *fn_val,
            None => panic!("Undeclared function: {}", fn_name.lexeme),
        };

        let args: Vec<BasicMetadataValueEnum> = params
            .iter()
            .map(|p| p.codegen(generator).into())
            .collect();

        let call_site = generator
            .builder
            .build_call(fn_val, &args, "calltmp")
            .unwrap();
        call_site.try_as_basic_value().expect_basic("expected value")
    }
}
