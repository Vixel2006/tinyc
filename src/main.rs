use tinyc::lexer::lexer::Lexer;

fn main() {
    let lexer = Lexer::new("int main() { return 0; }");

    for token in lexer {
        println!("{:?}", token);
    }
}
