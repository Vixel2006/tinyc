use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;

pub struct CodeGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new() -> Self {
        let context = Context::create();

        let module = context.create_module("tinyc");

        let builder = context.create_builder();

        let engine = module.create_execution_engine().unwrap();

        CodeGenerator {
            context,
            module,
            builder,
            engine,
        };
    }
}
