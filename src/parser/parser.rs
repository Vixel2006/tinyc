use super::productions::{
    BinaryOp, Decls, Expr, FuncDecl, Param, Program, Stmt, Type, UnaryOp, Val, VarDecl,
};
use crate::lexer::lexer::Lexer;
use crate::lexer::token::{Token, TokenKind};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: TokenKind,
        token: Token,
    },
    MissingToken {
        expected: TokenKind,
        found: TokenKind,
        token: Token,
    },
    CustomError(String),
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    previous_token: Token,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {}
