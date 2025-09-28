use tinyc::lexer::lexer::Lexer;
use tinyc::parser::parser::Parser;

fn main() {
    let lexer = Lexer::new("if (i <= 0 || i == 1) {\nint i = 0;\nchar y = '\t';\nreturn 0;\n}");
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_program();

    match ast {
        Ok(node) => println!("{:#?}", node),
        Err(e) => eprintln!("Error: {:?}", e),
    }
}
