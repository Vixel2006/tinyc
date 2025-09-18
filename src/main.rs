use tinyc::lexer::lexer::Lexer;

fn main() {
    let lexer = Lexer::new("int main() {\nint i = 0; if (i < 1) { print('GG'); }\nreturn 0;\n}");

    for token in lexer {
        println!("{:?}", token);
    }
}
