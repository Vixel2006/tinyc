use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;

use super::context::CodeGenerator;
use crate::parser::productions::{Decls, FuncDecl, Type, VarDecl};

impl Decls {
    pub fn codegen(&self, generator: &mut CodeGenerator) {
        match self {
            Decls::Var(var_decl) => var_decl.codegen(generator),
            Decls::Func(func_decl) => func_decl.codegen(generator),
        }
    }
}

impl FuncDecl {
    pub fn codegen(&self, generator: &mut CodeGenerator) {
        let param_types: Vec<BasicMetadataTypeEnum> = self
            .params
            .iter()
            .map(|p| generator.to_llvm_type(&p.param_type).into())
            .collect();

        let fn_type = match self.return_type {
            Type::Void => generator
                .context
                .void_type()
                .fn_type(&param_types, false),
            _ => {
                let ret_ty = generator.to_llvm_type(&self.return_type);
                match ret_ty {
                    BasicTypeEnum::IntType(ty) => ty.fn_type(&param_types, false),
                    BasicTypeEnum::FloatType(ty) => ty.fn_type(&param_types, false),
                    BasicTypeEnum::StructType(ty) => ty.fn_type(&param_types, false),
                    BasicTypeEnum::ArrayType(ty) => ty.fn_type(&param_types, false),
                    BasicTypeEnum::PointerType(ty) => ty.fn_type(&param_types, false),
                    BasicTypeEnum::VectorType(ty) => ty.fn_type(&param_types, false),
                    BasicTypeEnum::ScalableVectorType(ty) => ty.fn_type(&param_types, false),
                }
            }
        };

        let fn_val = generator
            .module
            .add_function(&self.identifier.lexeme, fn_type, None);

        generator
            .functions
            .insert(self.identifier.lexeme.clone(), (fn_val, self.return_type.clone()));

        if let Some(body) = &self.body {
            let entry_bb = generator.context.append_basic_block(fn_val, "entry");
            generator.builder.position_at_end(entry_bb);
            generator.current_function = Some(fn_val);

            for (i, param) in self.params.iter().enumerate() {
                let param_val = fn_val.get_nth_param(i as u32).unwrap();
                let alloca = generator
                    .builder
                    .build_alloca(
                        generator.to_llvm_type(&param.param_type),
                        &param.identifier.lexeme,
                    )
                    .unwrap();
                generator
                    .builder
                    .build_store(alloca, param_val)
                    .unwrap();
                generator.variables.insert(
                    param.identifier.lexeme.clone(),
                    (alloca, param.param_type.clone()),
                );
            }

            for stmt in body {
                stmt.codegen(generator);
            }

            if !generator.block_is_terminated() {
                match self.return_type {
                    Type::Void => {
                        generator.builder.build_return(None).unwrap();
                    }
                    Type::Int => {
                        let zero: BasicValueEnum = generator.context.i32_type().const_int(0, false).into();
                        generator
                            .builder
                            .build_return(Some(&zero))
                            .unwrap();
                    }
                    Type::Float => {
                        let zero: BasicValueEnum = generator.context.f64_type().const_float(0.0).into();
                        generator
                            .builder
                            .build_return(Some(&zero))
                            .unwrap();
                    }
                    Type::Bool => {
                        let zero: BasicValueEnum = generator.context.bool_type().const_int(0, false).into();
                        generator
                            .builder
                            .build_return(Some(&zero))
                            .unwrap();
                    }
                    Type::Char => {
                        let zero: BasicValueEnum = generator.context.i8_type().const_int(0, false).into();
                        generator
                            .builder
                            .build_return(Some(&zero))
                            .unwrap();
                    }
                    _ => {}
                }
            }

            generator.current_function = None;
        }
    }
}

impl VarDecl {
    pub fn codegen(&self, generator: &mut CodeGenerator) {
        let ty = generator.to_llvm_type(&self.var_type);
        let alloca = generator
            .builder
            .build_alloca(ty, &self.identifier.lexeme)
            .unwrap();

        if let Some(expr) = &self.initializer {
            let value = expr.codegen(generator);
            generator.builder.build_store(alloca, value).unwrap();
        }

        generator
            .variables
            .insert(self.identifier.lexeme.clone(), (alloca, self.var_type.clone()));
    }
}
