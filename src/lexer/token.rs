#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    If,
    Else,
    While,
    Int,
    Float,
    Bool,
    Char,
    Return,
    Void,

    // Whitespace
    Whitespace,

    // Identifiers
    Identifier,

    // Symbols
    Symbol,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftCurly,
    RightCurly,
    SingleQuote,
    DoubleQuote,

    // Operators
    Operator,
    Eq,
    Greater,
    Less,
    GEq,
    LEq,
    Plus,
    Minus,
    Multiply,
    Div,
    IsEq,

    // Terminator
    Terminator,

    // Unkown for errors
    Uknown,

    // Literals
    Integer,
    Decimal,
    Character,
    Boolean,

    // End of file
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
        Token {
            kind,
            lexeme: lexeme.to_string(),
            length,
            line,
            column,
        }
    }
}
