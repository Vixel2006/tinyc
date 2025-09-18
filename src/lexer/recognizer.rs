use super::token::{Token, TokenKind};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Start,
    Identifier,
    Whitespace,
    Symbol,
    Operator,
    Error,
}

pub struct DFA {
    keywords: HashMap<String, TokenKind>,
    symbols: HashMap<char, TokenKind>,
    operators: HashMap<String, TokenKind>,
}

impl DFA {
    fn init_keywords() -> HashMap<String, TokenKind> {
        let kw_strings: Vec<&str> = vec![
            "if", "else", "while", "int", "float", "char", "return", "void",
        ];

        let kw_tokens: Vec<TokenKind> = vec![
            TokenKind::If,
            TokenKind::Else,
            TokenKind::While,
            TokenKind::Int,
            TokenKind::Float,
            TokenKind::Char,
            TokenKind::Return,
            TokenKind::Void,
        ];

        let keywords: HashMap<String, TokenKind> = kw_strings
            .into_iter()
            .map(|s| s.to_string())
            .zip(kw_tokens.into_iter())
            .collect();

        keywords
    }

    fn init_symbols() -> HashMap<char, TokenKind> {
        let symbol_strings: Vec<char> = vec!['(', ')', '[', ']', '{', '}', '\'', '"', ';'];
        let symbol_tokens: Vec<TokenKind> = vec![
            TokenKind::LeftParen,
            TokenKind::RightParen,
            TokenKind::LeftBracket,
            TokenKind::RightBracket,
            TokenKind::LeftCurly,
            TokenKind::RightCurly,
            TokenKind::SingleQuote,
            TokenKind::DoubleQuote,
            TokenKind::Terminator,
        ];

        let symbols: HashMap<char, TokenKind> = symbol_strings
            .into_iter()
            .map(|x| x as char)
            .zip(symbol_tokens.into_iter())
            .collect();

        symbols
    }

    fn init_operators() -> HashMap<String, TokenKind> {
        let operator_strings: Vec<&str> = vec!["=", ">", "<", "==", "<=", ">=", "+", "-", "*", "/"];

        let operator_tokens: Vec<TokenKind> = vec![
            TokenKind::Eq,
            TokenKind::Greater,
            TokenKind::Less,
            TokenKind::IsEq,
            TokenKind::LEq,
            TokenKind::GEq,
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Multiply,
            TokenKind::Div,
        ];

        let operators: HashMap<String, TokenKind> = operator_strings
            .into_iter()
            .map(|x| x.to_string())
            .zip(operator_tokens.into_iter())
            .collect();

        operators
    }

    pub fn new() -> Self {
        DFA {
            keywords: Self::init_keywords(),
            symbols: Self::init_symbols(),
            operators: Self::init_operators(),
        }
    }

    fn is_symbol(c: char) -> bool {
        matches!(
            c,
            '(' | ')' | '{' | '}' | ';' | '\'' | '"' | '[' | ']' | ';'
        )
    }

    fn is_operator(op: char) -> bool {
        matches!(op, '=' | '<' | '>' | '+' | '-' | '*' | '/')
    }

    fn match_keywords(&self, buffer: &str, length: usize, line: u32, column: u32) -> Option<Token> {
        if let Some(token) = self.keywords.get(&buffer.to_string()) {
            return Some(Token {
                lexeme: buffer.to_string(),
                kind: token.clone(),
                length,
                line,
                column,
            });
        }
        None
    }

    fn match_symbols(&self, buffer: &str, length: usize, line: u32, column: u32) -> Option<Token> {
        if let Some(token) = self.symbols.get(&buffer.chars().next().unwrap()) {
            return Some(Token {
                lexeme: buffer.to_string(),
                kind: token.clone(),
                length,
                line,
                column,
            });
        }

        None
    }

    fn match_operator(&self, buffer: &str, length: usize, line: u32, column: u32) -> Option<Token> {
        if let Some(token) = self.operators.get(&buffer.to_string()) {
            return Some(Token {
                lexeme: buffer.to_string(),
                kind: token.clone(),
                length,
                line,
                column,
            });
        }

        None
    }

    pub fn recognize(&self, input: &str, line: u32, column: u32) -> Token {
        let mut chars = input.chars().peekable();
        let mut buffer = String::new();
        let mut current_state = State::Start;

        // Handle empty input
        let first_char = match chars.peek() {
            Some(&c) => c,
            None => {
                return Token {
                    lexeme: "".to_string(),
                    kind: TokenKind::Eof,
                    length: 0,
                    line,
                    column,
                };
            }
        };

        if first_char.is_ascii_alphabetic() {
            current_state = State::Identifier;
        } else if first_char.is_whitespace() {
            current_state = State::Whitespace;
        } else if DFA::is_symbol(first_char) {
            current_state = State::Symbol;
        } else if DFA::is_operator(first_char) {
            current_state = State::Operator;
        } else {
            current_state = State::Error;
        }

        buffer.push(chars.next().unwrap());

        while let Some(&c) = chars.peek() {
            match current_state {
                State::Identifier => {
                    if let Some(token) = self.match_keywords(&buffer, buffer.len(), line, column) {
                        return token;
                    }
                    if c.is_alphanumeric() {
                        buffer.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                State::Whitespace => {
                    if c.is_whitespace() {
                        buffer.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                State::Symbol => {
                    if let Some(token) = self.match_symbols(&buffer, buffer.len(), line, column) {
                        return token;
                    }
                }
                State::Operator => {
                    if let Some(token) = self.match_symbols(&buffer, buffer.len(), line, column) {
                        return token;
                    }
                }
                State::Error => {
                    break;
                }
                State::Start => {
                    break;
                }
            }
        }

        let kind = match current_state {
            State::Identifier => TokenKind::Identifier,
            State::Whitespace => TokenKind::Whitespace,
            State::Symbol => TokenKind::Symbol,
            State::Operator => TokenKind::Operator,
            State::Error => TokenKind::Uknown,
            State::Start => TokenKind::Uknown,
        };

        let token_length = buffer.len();
        Token {
            lexeme: buffer,
            kind,
            length: token_length,
            line,
            column,
        }
    }
}
