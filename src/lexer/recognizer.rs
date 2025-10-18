use super::token::{Token, TokenKind};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Identifier,
    Whitespace,
    Symbol,
    Operator,
    Punctuation,
    Integer,
    Float,
    Error,
    SusChar,
    SusStr,
    Char,
    Str,
}

#[derive(Debug, PartialEq)]
pub struct DFA {
    keywords: HashMap<String, TokenKind>,
    symbols: HashMap<char, TokenKind>,
    operators: HashMap<String, TokenKind>,
    punctuations: HashMap<char, TokenKind>,
}

impl DFA {
    fn init_keywords() -> HashMap<String, TokenKind> {
        let kw_strings: Vec<&str> = vec![
            "if", "else", "while", "int", "float", "char", "bool", "return", "void", "true",
            "false",
        ];

        let kw_tokens: Vec<TokenKind> = vec![
            TokenKind::If,
            TokenKind::Else,
            TokenKind::While,
            TokenKind::Int,
            TokenKind::Float,
            TokenKind::Char,
            TokenKind::Bool,
            TokenKind::Return,
            TokenKind::Void,
            TokenKind::True,
            TokenKind::False,
        ];

        kw_strings
            .into_iter()
            .map(|s| s.to_string())
            .zip(kw_tokens.into_iter())
            .collect()
    }

    fn init_symbols() -> HashMap<char, TokenKind> {
        let symbol_strings: Vec<char> = vec!['(', ')', '[', ']', '{', '}', '\'', '"'];
        let symbol_tokens: Vec<TokenKind> = vec![
            TokenKind::LeftParen,
            TokenKind::RightParen,
            TokenKind::LeftBracket,
            TokenKind::RightBracket,
            TokenKind::LeftCurly,
            TokenKind::RightCurly,
            TokenKind::SingleQuote,
            TokenKind::DoubleQuote,
        ];

        symbol_strings
            .into_iter()
            .zip(symbol_tokens.into_iter())
            .collect()
    }

    fn init_operators() -> HashMap<String, TokenKind> {
        let operator_strings: Vec<&str> = vec![
            "=", ">", "<", "==", "!=", "<=", ">=", "+", "-", "*", "/", "&&", "||", "!", "&", "|", "~",
            "^", "<<", ">>", "+=", "-=", "*=", "/=",
        ];

        let operator_tokens: Vec<TokenKind> = vec![
            TokenKind::Eq,
            TokenKind::Greater,
            TokenKind::Less,
            TokenKind::IsEq,
            TokenKind::NotEq,
            TokenKind::LEq,
            TokenKind::GEq,
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Multiply,
            TokenKind::Div,
            TokenKind::LogicalAnd,
            TokenKind::LogicalOr,
            TokenKind::LogicalNot,
            TokenKind::BitwiseAnd,
            TokenKind::BitwiseOr,
            TokenKind::BitwiseNot,
            TokenKind::BitwiseXor,
            TokenKind::LeftShift,
            TokenKind::RightShift,
            TokenKind::PlusEq,
            TokenKind::MinusEq,
            TokenKind::MultiplyEq,
            TokenKind::DivEq,
        ];

        operator_strings
            .into_iter()
            .map(|s| s.to_string())
            .zip(operator_tokens.into_iter())
            .collect()
    }

    fn init_punctuation() -> HashMap<char, TokenKind> {
        let punctuation_strings: Vec<char> = vec![';', ',', '.', ':'];
        let punctuation_tokens: Vec<TokenKind> = vec![
            TokenKind::SemiColon,
            TokenKind::Comma,
            TokenKind::Dot,
            TokenKind::Colon,
        ];

        punctuation_strings
            .into_iter()
            .zip(punctuation_tokens.into_iter())
            .collect()
    }

    pub fn new() -> Self {
        DFA {
            keywords: Self::init_keywords(),
            symbols: Self::init_symbols(),
            operators: Self::init_operators(),
            punctuations: Self::init_punctuation(),
        }
    }

    fn is_punctuation(c: char) -> bool {
        matches!(c, ';' | ',' | '.' | ':')
    }

    fn is_symbol(c: char) -> bool {
        matches!(c, '(' | ')' | '{' | '}' | '[' | ']')
    }

    fn is_single_quote(c: char) -> bool {
        matches!(c, '\'')
    }

    fn is_double_quote(c: char) -> bool {
        matches!(c, '"')
    }

    fn is_escape_sequence(first: char, second: Option<char>) -> bool {
        if first == '\\' {
            return false;
        }

        match second {
            Some('\'') | Some('\\') | Some('n') | Some('t') | Some('\"') => true,
            _ => false,
        }
    }

    fn is_operator(op: char) -> bool {
        matches!(
            op,
            '=' | '<' | '>' | '+' | '-' | '*' | '/' | '|' | '&' | '^' | '~' | '!'
        )
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

    fn match_punctuation(
        &self,
        buffer: &str,
        length: usize,
        line: u32,
        column: u32,
    ) -> Option<Token> {
        buffer.chars().next().and_then(|c| {
            self.punctuations.get(&c).map(|token| Token {
                lexeme: buffer.to_string(),
                kind: token.clone(),
                length,
                line,
                column,
            })
        })
    }

    pub fn recognize(&self, input: &str, line: u32, column: u32) -> Token {
        let mut chars = input.chars().peekable();
        let mut buffer = String::new();

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

        let mut current_state = if first_char.is_ascii_alphabetic() {
            State::Identifier
        } else if first_char.is_ascii_digit() {
            State::Integer
        } else if first_char.is_whitespace() {
            State::Whitespace
        } else if DFA::is_single_quote(first_char) {
            State::SusChar
        } else if DFA::is_double_quote(first_char) {
            State::SusStr
        } else if DFA::is_symbol(first_char) {
            State::Symbol
        } else if DFA::is_operator(first_char) {
            State::Operator
        } else if DFA::is_punctuation(first_char) {
            State::Punctuation
        } else {
            State::Error
        };

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
                    let mut temp = buffer.clone();
                    temp.push(c);
                    if self.operators.contains_key(&temp) {
                        buffer.push(c);
                        chars.next();
                    }
                    break;
                }
                State::Integer => {
                    if c.is_ascii_digit() {
                        buffer.push(c);
                        chars.next();
                    } else if c == '.' {
                        current_state = State::Float;
                        buffer.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                State::Float => {
                    if c.is_ascii_digit() {
                        buffer.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                State::SusChar => {
                    if (c.is_alphanumeric()
                        || DFA::is_escape_sequence(c, chars.peek().copied())
                        || c.is_whitespace())
                        && buffer.len() == 1
                    {
                        buffer.push(c);
                        chars.next();
                    } else if DFA::is_single_quote(c) && buffer.len() == 2 {
                        buffer.push(c);
                        chars.next();
                        current_state = State::Char
                    } else {
                        break;
                    }
                }
                State::SusStr => {
                    if c.is_alphanumeric() || DFA::is_escape_sequence(c, chars.peek().copied()) {
                        buffer.push(c);
                        chars.next();
                    } else if DFA::is_double_quote(c) {
                        buffer.push(c);
                        chars.next();
                        current_state = State::Str;
                    } else {
                        break;
                    }
                }
                State::Symbol
                | State::Char
                | State::Str
                | State::Punctuation
                | State::Error => {
                    break;
                }
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
            State::Integer => Token {
                lexeme: buffer,
                kind: TokenKind::Integer,
                length: token_length,
                line,
                column,
            },
            State::Float => Token {
                lexeme: buffer,
                kind: TokenKind::Decimal,
                length: token_length,
                line,
                column,
            },
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
            State::Punctuation => self
                .match_punctuation(&buffer, token_length, line, column)
                .unwrap_or(Token {
                    lexeme: buffer,
                    kind: TokenKind::Symbol,
                    length: token_length,
                    line,
                    column,
                }),
            State::Char => Token {
                lexeme: buffer,
                kind: TokenKind::Character,
                length: token_length,
                line,
                column,
            },

            State::Str => Token {
                lexeme: buffer,
                kind: TokenKind::String,
                length: token_length,
                line,
                column,
            },
            State::Error | State::SusStr | State::SusChar => Token {
                lexeme: buffer,
                kind: TokenKind::Unknown,
                length: token_length,
                line,
                column,
            },
        }
    }
}
