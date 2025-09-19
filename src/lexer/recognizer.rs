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
    Eof,
}

#[derive(Debug, PartialEq)]
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

        kw_strings
            .into_iter()
            .map(|s| s.to_string())
            .zip(kw_tokens.into_iter())
            .collect()
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

        symbol_strings
            .into_iter()
            .zip(symbol_tokens.into_iter())
            .collect()
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

        operator_strings
            .into_iter()
            .map(|s| s.to_string())
            .zip(operator_tokens.into_iter())
            .collect()
    }

    pub fn new() -> Self {
        DFA {
            keywords: Self::init_keywords(),
            symbols: Self::init_symbols(),
            operators: Self::init_operators(),
        }
    }

    fn is_symbol(c: char) -> bool {
        matches!(c, '(' | ')' | '{' | '}' | ';' | '\'' | '"' | '[' | ']')
    }

    fn is_operator(op: char) -> bool {
        matches!(op, '=' | '<' | '>' | '+' | '-' | '*' | '/')
    }

    fn match_keywords(&self, buffer: &str, length: usize, line: u32, column: u32) -> Option<Token> {
        self.keywords.get(buffer).map(|token| Token {
            lexeme: buffer.to_string(),
            kind: token.clone(),
            length,
            line,
            column,
        })
    }

    fn match_symbols(&self, buffer: &str, length: usize, line: u32, column: u32) -> Option<Token> {
        buffer.chars().next().and_then(|c| {
            self.symbols.get(&c).map(|token| Token {
                lexeme: buffer.to_string(),
                kind: token.clone(),
                length,
                line,
                column,
            })
        })
    }

    fn match_operator(&self, buffer: &str, length: usize, line: u32, column: u32) -> Option<Token> {
        self.operators.get(buffer).map(|token| Token {
            lexeme: buffer.to_string(),
            kind: token.clone(),
            length,
            line,
            column,
        })
    }

    pub fn recognize(&self, input: &str, line: u32, column: u32) -> Token {
        let mut chars = input.chars().peekable();
        let mut buffer = String::new();
        let mut current_state = State::Start;

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
                    if c.is_alphanumeric() || c == '_' {
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
                State::Operator => {
                    // allow 2-char ops like ==, <=, >=
                    let mut temp = buffer.clone();
                    temp.push(c);
                    if self.operators.contains_key(&temp) {
                        buffer.push(c);
                        chars.next();
                    }
                    break;
                }
                State::Symbol | State::Error | State::Start | State::Eof => break,
            }
        }

        let token_length = buffer.len();

        // final classification
        match current_state {
            State::Identifier => {
                if let Some(keyword) = self.match_keywords(&buffer, token_length, line, column) {
                    return keyword;
                }
                Token {
                    lexeme: buffer,
                    kind: TokenKind::Identifier,
                    length: token_length,
                    line,
                    column,
                }
            }
            State::Whitespace => Token {
                lexeme: buffer,
                kind: TokenKind::Whitespace,
                length: token_length,
                line,
                column,
            },
            State::Symbol => self
                .match_symbols(&buffer, token_length, line, column)
                .unwrap_or(Token {
                    lexeme: buffer,
                    kind: TokenKind::Symbol,
                    length: token_length,
                    line,
                    column,
                }),
            State::Operator => self
                .match_operator(&buffer, token_length, line, column)
                .unwrap_or(Token {
                    lexeme: buffer,
                    kind: TokenKind::Operator,
                    length: token_length,
                    line,
                    column,
                }),
            State::Error => Token {
                lexeme: buffer,
                kind: TokenKind::Uknown,
                length: token_length,
                line,
                column,
            },
            _ => Token {
                lexeme: "".to_string(),
                kind: TokenKind::Eof,
                length: 0,
                line,
                column,
            },
        }
    }
}
