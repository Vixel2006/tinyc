use super::context::CodeGenerator;
use crate::parser::productions::Stmt;

impl<'ctx> Stmt {
    pub fn codegen(&self, generator: &mut CodeGenerator<'ctx>) {
        match self {
            Stmt::Decl(decls) => {
                decls.codegen(generator);
            }
            Stmt::Assign(identifier, expr) => {
                let val = expr.codegen(generator);
                if let Some((alloca, _)) = generator.variables.get(&identifier.lexeme) {
                    generator.builder.build_store(*alloca, val).unwrap();
                } else {
                    panic!("Undeclared variable: {}", identifier.lexeme);
                }
            }
            Stmt::Expr(expr) => {
                expr.codegen(generator);
            }
            Stmt::Return(expr) => match expr {
                Some(e) => {
                    let val = e.codegen(generator);
                    generator.builder.build_return(Some(&val)).unwrap();
                }
                None => {
                    generator.builder.build_return(None).unwrap();
                }
            },
            Stmt::If(condition, then_stmts, else_stmts) => {
                let cond_val = condition.codegen(generator);
                let cond_i1 = generator.build_truthy(cond_val, "ifcond");

                let current_fn = generator.current_function.expect("No current function");

                let then_bb = generator.context.append_basic_block(current_fn, "then");
                let else_bb = generator.context.append_basic_block(current_fn, "else");
                let merge_bb = generator.context.append_basic_block(current_fn, "ifcont");

                generator
                    .builder
                    .build_conditional_branch(cond_i1, then_bb, else_bb)
                    .unwrap();

                generator.builder.position_at_end(then_bb);
                for stmt in then_stmts {
                    stmt.codegen(generator);
                }
                if !generator.block_is_terminated() {
                    generator
                        .builder
                        .build_unconditional_branch(merge_bb)
                        .unwrap();
                }

                generator.builder.position_at_end(else_bb);
                if let Some(else_stmts) = else_stmts {
                    for stmt in else_stmts {
                        stmt.codegen(generator);
                    }
                }
                if !generator.block_is_terminated() {
                    generator
                        .builder
                        .build_unconditional_branch(merge_bb)
                        .unwrap();
                }

                generator.builder.position_at_end(merge_bb);
            }
            Stmt::While(condition, body_stmts) => {
                let current_fn = generator.current_function.expect("No current function");

                let cond_bb = generator.context.append_basic_block(current_fn, "whilecond");
                let body_bb = generator.context.append_basic_block(current_fn, "whilebody");
                let end_bb = generator.context.append_basic_block(current_fn, "whileend");

                generator
                    .builder
                    .build_unconditional_branch(cond_bb)
                    .unwrap();

                generator.builder.position_at_end(cond_bb);
                let cond_val = condition.codegen(generator);
                let cond_i1 = generator.build_truthy(cond_val, "whilecond");
                generator
                    .builder
                    .build_conditional_branch(cond_i1, body_bb, end_bb)
                    .unwrap();

                generator.builder.position_at_end(body_bb);
                for stmt in body_stmts {
                    stmt.codegen(generator);
                }
                if !generator.block_is_terminated() {
                    generator
                        .builder
                        .build_unconditional_branch(cond_bb)
                        .unwrap();
                }

                generator.builder.position_at_end(end_bb);
            }
            Stmt::Block(stmts) => {
                for stmt in stmts {
                    stmt.codegen(generator);
                }
            }
        }
    }
}
