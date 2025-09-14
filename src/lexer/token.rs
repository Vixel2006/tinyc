#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Identifier,
    Keyword,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub length: usize,
    pub line: usize,
    pub column: usize,
}
