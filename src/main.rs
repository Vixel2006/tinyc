use tinyc::lexer::lexer::Lexer;

fn main() {
    let lexer = Lexer::new("if (i 0) return 0;");

    for token in lexer {
        println!("{:?}", token);
    }
}
