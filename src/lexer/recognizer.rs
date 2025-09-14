use super::token::{Token, TokenKind};

pub struct NFA<'a> {
    input: &'a str,
    pos: usize,
    line: usize,
    column: usize,
}
