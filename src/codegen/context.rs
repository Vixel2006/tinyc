use crate::parser::productions::Type;
use inkwell::{
    builder::Builder, context::Context, module::Module, passes::PassManager, types::BasicTypeEnum,
    values::BasicValueEnum, values::FunctionValue, values::IntValue, values::PointerValue,
    FloatPredicate, IntPredicate,
};
use std::collections::HashMap;

pub struct CodeGenerator<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub fpm: PassManager<FunctionValue<'ctx>>,
    pub variables: HashMap<String, (PointerValue<'ctx>, Type)>,
    pub functions: HashMap<String, (FunctionValue<'ctx>, Type)>,
    pub current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let fpm = PassManager::create(&module);
        CodeGenerator {
            context,
            module,
            builder,
            fpm,
            variables: HashMap::new(),
            functions: HashMap::new(),
            current_function: None,
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

    pub fn build_truthy(&self, val: BasicValueEnum<'ctx>, name: &str) -> IntValue<'ctx> {
        match val {
            BasicValueEnum::IntValue(int_val) => {
                if int_val.get_type().get_bit_width() == 1 {
                    int_val
                } else {
                    let zero = int_val.get_type().const_int(0, false);
                    self.builder
                        .build_int_compare(IntPredicate::NE, int_val, zero, name)
                        .unwrap()
                }
            }
            BasicValueEnum::FloatValue(float_val) => {
                let zero = float_val.get_type().const_float(0.0);
                self.builder
                    .build_float_compare(FloatPredicate::ONE, float_val, zero, name)
                    .unwrap()
            }
            _ => panic!("Cannot convert to truthy value"),
        }
    }

    pub fn block_is_terminated(&self) -> bool {
        self.builder
            .get_insert_block()
            .map_or(true, |bb| bb.get_terminator().is_some())
    }


}
