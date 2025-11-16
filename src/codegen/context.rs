use crate::parser::productions::Type;
use inkwell::{
    builder::Builder, context::Context, module::Module, passes::PassManager, types::BasicTypeEnum,
    values::FunctionValue, values::PointerValue,
};
use std::collections::HashMap;

pub struct CodeGenerator<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub fpm: PassManager<FunctionValue<'ctx>>,
    pub variables: HashMap<String, (PointerValue<'ctx>, Type)>,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        let fpm = PassManager::create(&module);

        let variables = HashMap::new();

        CodeGenerator {
            context,
            module,
            builder,
            fpm,
            variables,
        }
    }

    pub fn to_llvm_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Int => self.context.i32_type().into(),
            Type::Float => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Char => self.context.i8_type().into(),
            _ => panic!("Void type can't be used as value type"),
        }
    }
}
