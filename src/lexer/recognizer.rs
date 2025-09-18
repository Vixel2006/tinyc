use super::token::{Token, TokenKind};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Start,
    Identifier,
    Whitespace,
    Symbol,
    Error,
}

pub struct DFA {
    keywords: HashMap<String, TokenKind>,
}

impl DFA {
    pub fn new() -> Self {
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

        DFA { keywords }
    }

    fn is_symbol(c: char) -> bool {
        matches!(c, '(' | ')' | '{' | '}' | ';' | '+' | '-' | '*' | '/' | '=')
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
                    break;
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
