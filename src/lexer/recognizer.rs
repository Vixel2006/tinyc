use super::token::{Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Start,
    Identifier,
    Whitespace,
    Symbol,
    Error,
}

pub struct DFA;

impl DFA {
    fn is_symbol(c: char) -> bool {
        matches!(c, '(' | ')' | '{' | '}' | ';' | '+' | '-' | '*' | '/' | '=')
    }

    pub fn recognize(input: &str, line: u32, column: u32) -> Token {
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
