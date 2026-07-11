use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;
use tinyc::codegen::context::CodeGenerator;
use tinyc::lexer::lexer::Lexer;
use tinyc::parser::parser::Parser;

fn compile(input_path: &str, output_path: &str) {
    let source_code = match fs::read_to_string(input_path) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error reading file {}: {}", input_path, e);
            std::process::exit(1);
        }
    };

    let lexer = Lexer::new(&source_code);
    let tokens = lexer.collect::<Vec<_>>();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program();

    let program = match ast {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Error: {:?}", e);
            std::process::exit(1);
        }
    };

    let context = inkwell::context::Context::create();
    let mut generator = CodeGenerator::new(&context, "tinyc");

    for decl in &program.declarations {
        decl.codegen(&mut generator);
    }

    inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default())
        .unwrap();

    let triple = inkwell::targets::TargetMachine::get_default_triple();
    let target = inkwell::targets::Target::from_triple(&triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            inkwell::OptimizationLevel::Aggressive,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap();

    let options = inkwell::passes::PassBuilderOptions::create();
    generator
        .module
        .run_passes("default<O3>", &target_machine, options)
        .unwrap();

    let obj_path = format!("{}.o", output_path);
    let obj_file = Path::new(&obj_path);

    target_machine
        .write_to_file(&generator.module, inkwell::targets::FileType::Object, obj_file)
        .unwrap();

    let status = Command::new("cc")
        .args([obj_file.to_str().unwrap(), "-o", output_path])
        .status()
        .expect("failed to link with cc");

    if !status.success() {
        eprintln!("Linker failed");
        std::process::exit(1);
    }

    let _ = fs::remove_file(&obj_path);
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage:");
        eprintln!("  {} <input.tinyc>             compile to executable", args[0]);
        eprintln!("  {} <input.tinyc> -o <output>  compile to <output>", args[0]);
        eprintln!("  {} <input.tinyc> --emit-ir    dump LLVM IR", args[0]);
        std::process::exit(1);
    }

    let input_path = &args[1];

    if args.len() > 2 && args[2] == "--emit-ir" {
        let source_code = match fs::read_to_string(input_path) {
            Ok(code) => code,
            Err(e) => {
                eprintln!("Error reading file {}: {}", input_path, e);
                std::process::exit(1);
            }
        };

        let lexer = Lexer::new(&source_code);
        let tokens = lexer.collect::<Vec<_>>();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse_program();

        match ast {
            Ok(program) => {
                let context = inkwell::context::Context::create();
                let mut generator = CodeGenerator::new(&context, "tinyc");

                for decl in &program.declarations {
                    decl.codegen(&mut generator);
                }

                generator.module.print_to_stderr();
            }
            Err(e) => eprintln!("Error: {:?}", e),
        }
        return;
    }

    let output_path = if args.len() > 2 && args[2] == "-o" {
        if args.len() < 4 {
            eprintln!("Error: -o requires an output path");
            std::process::exit(1);
        }
        args[3].clone()
    } else {
        let input = Path::new(input_path);
        input.with_extension("").to_string_lossy().to_string()
    };

    compile(input_path, &output_path);
}
