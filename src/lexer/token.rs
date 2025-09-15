

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    If,
    Else,
    While,
    Int,
    Float,
    Bool,
    Char,
    Return,
    Void,
    Whitespace,
    Identifier,
    Symbol,
    Terminator,
    Uknown,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub length: usize,
    pub line: u32,
    pub column: u32,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: &str, length: usize, line: u32, column: u32) -> Self {
        Token { kind, lexeme: lexeme.to_string(), length, line, column }
    }
}

