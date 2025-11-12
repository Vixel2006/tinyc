use tinyc::codegen::context::CodeGenerator;
use tinyc::lexer::lexer::Lexer;
use tinyc::parser::parser::Parser;

fn main() {
    let lexer = Lexer::new("int main() {\nif (i <= 0) {\nint i = 0;\nreturn 0;\n}\n}");
    let tokens = lexer.collect::<Vec<_>>();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program();

    match ast {
        Ok(node) => println!("{:#?}", node),
        Err(e) => eprintln!("Error: {:?}", e),
    }
}
