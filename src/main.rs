use tinyc::lexer::lexer::Lexer;

fn main() {
    let lexer = Lexer::new("if (i <= 0 || i == 1) { int i = 0; i = i << 1; return 0; }");

    for token in lexer {
        println!("{:?}", token);
    }
}
