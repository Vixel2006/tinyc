use tinyc::lexer::lexer::Lexer;
use tinyc::lexer::token::{Token, TokenKind};

fn main() {
    let file = "int main() { return 0; }";

    let lexer = Lexer::new(&file);

    for token in lexer {
        println!("{:?}", token);
    }
}
