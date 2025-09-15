use super::recognizer::DFA;
use super::token::{Token, TokenKind};

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    line: u32,
    col: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            line: 1,
            col: 1,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.input.len() {
            return Some(Token::new(
                TokenKind::Eof,
                "",
                self.pos,
                self.line,
                self.col,
            ));
        }

        let remaining = &self.input[self.pos..];
        let token = DFA::recognize(remaining, self.line, self.col);

        self.pos += token.length;
        self.col += token.length as u32;

        if token.kind == TokenKind::Whitespace && token.lexeme.contains('\n') {
            self.line += 1;
            self.col = 1;
        }

        Some(token)
    }
}
