use super::context::CodeGenerator;
use crate::parser::productions::{Decls, FuncDecl, VarDecl};

impl Decls {
    fn codegen(&self, generator: &mut CodeGenerator) {
        match self {
            Decls::Var(var_decl) => var_decl.codegen(generator),
            Decls::Func(func_decl) => func_decl.codegen(generator),
        }
    }
}

impl FuncDecl {
    fn codegen(&self, _generator: &mut CodeGenerator) {
        // TODO: Implement function declaration codegen
        todo!()
    }
}

impl VarDecl {
    fn codegen(&self, generator: &mut CodeGenerator) {
        let ty = generator.to_llvm_type(&self.var_type);
        let alloca = generator
            .builder
            .build_alloca(ty, &self.identifier.lexeme)
            .unwrap(); // Unwrap the result of build_alloca

        if let Some(expr) = &self.initializer {
            let value = expr.codegen(generator); // expr.codegen now returns BasicValueEnum
            generator.builder.build_store(alloca, value).unwrap();
        }

        generator
            .variables
            .insert(self.identifier.lexeme.clone(), alloca);
    }
}
