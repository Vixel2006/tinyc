use tinyc::lexer::lexer::Lexer;

fn main() {
    let lexer = Lexer::new("if (i <= 0 || i == 1) {\nint i = 0;\nchar y = '\t';\nreturn 0;\n}");

    for token in lexer {
        println!("{:?}", token);
    }
}
