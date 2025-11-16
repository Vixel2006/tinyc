use std::env;
use std::fs;
use tinyc::codegen::context::CodeGenerator;
use tinyc::lexer::lexer::Lexer;
use tinyc::parser::parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <input_file.tinyc>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    let source_code = match fs::read_to_string(file_path) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error reading file {}: {}", file_path, e);
            std::process::exit(1);
        }
    };

    let lexer = Lexer::new(&source_code);
    let tokens = lexer.collect::<Vec<_>>();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program();

    match ast {
        Ok(node) => println!("{:#?}", node),
        Err(e) => eprintln!("Error: {:?}", e),
    }
}
