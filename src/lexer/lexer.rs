use super::recognizer::DFA;
use super::token::{Token, TokenKind};

#[derive(Debug, PartialEq)]
pub struct Lexer<'a> {
    dfa: DFA,
    input: &'a str,
    pos: usize,
    line: u32,
    col: u32,
    returned_eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            dfa: DFA::new(),
            input,
            pos: 0,
            line: 1,
            col: 1,
            returned_eof: false,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.pos >= self.input.len() {
                if !self.returned_eof {
                    self.returned_eof = true;

                    return Some(Token {
                        lexeme: "".to_string(),
                        kind: TokenKind::Eof,
                        length: 0,
                        line: self.line,
                        column: self.col,
                    });
                } else {
                    return None;
                }
            }

            let remaining = &self.input[self.pos..];
            let token = self.dfa.recognize(remaining, self.line, self.col);

            self.pos += token.length;
            self.col += token.length as u32;

            if token.kind == TokenKind::Whitespace {
                if token.lexeme.contains('\n') {
                    self.line += 1;
                    self.col = 1;
                }
                continue; // Skip whitespace and get the next token
            }

            return Some(token);
        }
    }
}
