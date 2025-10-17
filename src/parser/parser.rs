use super::productions::{
    BinaryOp, Decls, Expr, FuncDecl, Param, Program, Stmt, Type, UnaryOp, Val, VarDecl,
};
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
    tokens: &'a [Token],
    curr: usize,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn matches(&mut self, kind: TokenKind) -> Result<bool, String> {
        if self.tokens[self.curr].kind == kind {
            self.consume();
            Ok(true)
        } else {
            Err(format!(
                "Syntax Error: line: {}, col: {}, Expected {:?} but got {:?}",
                self.tokens[self.curr].line,
                self.tokens[self.curr].column,
                kind,
                self.tokens[self.curr].kind
            ))
        }
    }

    fn advance(&mut self) {
        if self.curr < self.tokens.len() - 1 {
            self.curr += 1;
        }
    }

    fn consume(&mut self) {
        if self.tokens[self.curr].kind != TokenKind::Eof {
            self.advance();
        }
    }

    fn match_tokens(&self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if &self.tokens[self.curr].kind == kind {
                return true;
            }
        }
        return false;
    }

    fn parse_stmt(&mut self) {}

    fn parse_expr(&mut self) {}

    fn parse_decl(&mut self) -> Result<Decls, String> {
        let mut ty: Type;
        let mut id: Token;
        if self.match_tokens(&[
            TokenKind::Int,
            TokenKind::Float,
            TokenKind::Bool,
            TokenKind::Void,
            TokenKind::Char,
        ]) {
            ty = match self.tokens[self.curr].kind {
                TokenKind::Int => Type::Int,
                TokenKind::Float => Type::Float,
                TokenKind::Char => Type::Char,
                TokenKind::Bool => Type::Bool,
                TokenKind::Void => Type::Void,
                default => Type::Unknown,
            };
            self.consume();
        }

        if self.matches(TokenKind::Identifier).unwrap() {
            id = self.tokens[self.curr];
            self.consume();
        }

        if self.matches(TokenKind::LeftParen).unwrap() {
            // Here we go for the func decl shit
        } else if self.match_tokens(&[TokenKind::SemiColon, TokenKind::Eq]) {
            // Here we go for the variable decl shit
        } else {
            // Fuck you stupid pitch (aka. raise an error)
        }
    }

    fn parse_var_decl(&mut self, ty: Type, id: &Token) -> Result<VarDecl, String> {
        if self.matches(TokenKind::SemiColon).unwrap() {
            return Ok(VarDecl {
                var_type: ty,
                identifier: id,
                initializer: None,
            });
        } else if self.matches(TokenKind::Eq).unwrap() {
            self.consume();
            // Here we do some stupid experision parsing
        }
    }

    fn parse_func_decl(&mut self, ty: Type, id: &Token) -> Result<FuncDecl, String> {
        let mut params: Vec<Param> = Vec::new();
        if self.tokens[self.curr].kind != TokenKind::RightParen {
            loop {
                let param = self.parse_param();
                params.push(param.unwrap());
                self.consume();

                if self.tokens[self.curr].kind == TokenKind::Comma {
                    self.consume();
                    continue;
                } else {
                    break;
                }
            }
        } else {
            self.consume();
        }

        if self.match_tokens(&[TokenKind::SemiColon]) {
            self.consume();
            Ok(FuncDecl {
                return_type: ty,
                identifier: id,
                params: params,
                body: None,
            })
        } else {
            Ok(FuncDecl {
                return_type: ty,
                identifier: id,
                params,
                body: None,
            })
        }
    }

    fn parse_param(&mut self) -> Result<Param, String> {
        let ty: Type;
        let id: &Token;

        if self.match_tokens(&[
            TokenKind::Int,
            TokenKind::Float,
            TokenKind::Bool,
            TokenKind::Void,
            TokenKind::Char,
        ]) {
            ty = match self.tokens[self.curr].kind {
                TokenKind::Int => Type::Int,
                TokenKind::Float => Type::Float,
                TokenKind::Char => Type::Char,
                TokenKind::Bool => Type::Bool,
                TokenKind::Void => Type::Void,
                default => Type::Unknown,
            };
            self.consume();
        } else {
            return Err(format!("not correct parameter"));
        }

        if self.matches(TokenKind::Identifier).unwrap() {
            id = &self.tokens[self.curr];
            self.consume();
        }

        Ok(Param {
            param_type: ty,
            identifier: id,
        })
    }
}
