use tinyc::lexer::token::{Token, TokenKind};

fn main() {
    let token = Token {
        kind: TokenKind::Keyword,
        lexeme: "if".to_string(),
        length: 2,
        line: 1,
        column: 0,
    };
    println!("Hello, {:?}", token);
}
