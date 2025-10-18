use super::productions::{Decls, Expr, FuncDecl, Param, Program, Stmt, Type, Val, VarDecl};
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

pub struct Parser {
    pub tokens: Vec<Token>,
    pub curr: usize,
    pub errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens,
            curr: 0,
            errors: Vec::new(),
        }
    }

    fn consume_if_matches(&mut self, kind: TokenKind) -> bool {
        if self.tokens[self.curr].kind == kind {
            self.consume();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind, error_message: &str) -> Result<(), String> {
        if self.tokens[self.curr].kind == kind {
            self.consume();
            Ok(())
        } else {
            Err(format!(
                "Syntax Error: line: {}, col: {}, {}. Expected {:?} but got {:?}",
                self.tokens[self.curr].line,
                self.tokens[self.curr].column,
                error_message,
                kind,
                self.tokens[self.curr].kind
            ))
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.tokens[self.curr].kind == kind
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

    fn peek(&self) -> TokenKind {
        self.tokens[self.curr].kind.clone()
    }

    fn match_tokens(&self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if &self.tokens[self.curr].kind == kind {
                return true;
            }
        }
        return false;
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt, String> {}

    pub fn parse_expr(&mut self) -> Result<Expr, String> {
        Ok(Expr::Value(Val::Integer(0)))
    }

    pub fn parse_decl(&mut self) -> Result<Decls, String> {
        let ty: Type;
        if self.match_tokens(&[
            TokenKind::Int,
            TokenKind::Float,
            TokenKind::Bool,
            TokenKind::Void,
            TokenKind::Char,
        ]) {
            ty = match &self.tokens[self.curr].kind {
                TokenKind::Int => Type::Int,
                TokenKind::Float => Type::Float,
                TokenKind::Char => Type::Char,
                TokenKind::Bool => Type::Bool,
                TokenKind::Void => Type::Void,
                _default => Type::Unknown,
            };
            self.consume();
        } else {
            return Err(format!(
                "Unexpected token '{}' at line {}, column {} - expected a data type",
                self.tokens[self.curr].lexeme,
                self.tokens[self.curr].line,
                self.tokens[self.curr].column
            ));
        }

        self.expect(TokenKind::Identifier, "expected an identifier")?;
        let id = self.tokens[self.curr - 1].clone();

        if self.consume_if_matches(TokenKind::LeftParen) {
            return self.parse_func_decl(ty, id).map(Decls::Func);
        } else if self.match_tokens(&[TokenKind::SemiColon, TokenKind::Eq]) {
            return self.parse_var_decl(ty, id).map(Decls::Var);
        } else {
            return Err(format!(
                "Unexpected token '{}' at line {}, column {} — expected '(', ';', or '=' after identifier",
                self.tokens[self.curr].lexeme,
                self.tokens[self.curr].line,
                self.tokens[self.curr].column
            ));
        }
    }

    fn parse_var_decl(&mut self, ty: Type, id: Token) -> Result<VarDecl, String> {
        if self.consume_if_matches(TokenKind::SemiColon) {
            return Ok(VarDecl {
                var_type: ty,
                identifier: id,
                initializer: None,
            });
        } else if self.consume_if_matches(TokenKind::Eq) {
            let expr = self.parse_expr();

            match expr {
                Ok(value) => Ok(VarDecl {
                    var_type: ty,
                    identifier: id,
                    initializer: Some(value),
                }),
                Err(error) => Err(error),
            }
        } else {
            return Err(format!(
                "Unexpected token '{}' at line {}, column {} - expected ';' or '=' after identifier",
                self.tokens[self.curr].lexeme,
                self.tokens[self.curr].line,
                self.tokens[self.curr].column,
            ));
        }
    }

    fn parse_func_decl(&mut self, ty: Type, id: Token) -> Result<FuncDecl, String> {
        let mut params: Vec<Param> = Vec::new();
        if !self.check(TokenKind::RightParen) {
            loop {
                params.push(self.parse_param()?);

                if self.consume_if_matches(TokenKind::Comma) {
                    continue;
                } else {
                    break;
                }
            }
        }
        self.expect(TokenKind::RightParen, "expected ')' after parameters")?;

        if self.consume_if_matches(TokenKind::SemiColon) {
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

        if self.match_tokens(&[
            TokenKind::Int,
            TokenKind::Float,
            TokenKind::Bool,
            TokenKind::Void,
            TokenKind::Char,
        ]) {
            ty = match &self.tokens[self.curr].kind {
                TokenKind::Int => Type::Int,
                TokenKind::Float => Type::Float,
                TokenKind::Char => Type::Char,
                TokenKind::Bool => Type::Bool,
                TokenKind::Void => Type::Void,
                _default => Type::Unknown,
            };
            self.consume();
        } else {
            return Err(format!(
                "Unexpected token '{}' at line {}, column {} - expected a data type",
                self.tokens[self.curr].lexeme,
                self.tokens[self.curr].line,
                self.tokens[self.curr].column
            ));
        }

        self.expect(TokenKind::Identifier, "expected an identifier")?;
        let id = self.tokens[self.curr - 1].clone();

        Ok(Param {
            param_type: ty,
            identifier: id,
        })
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut decls: Vec<Decls> = Vec::new();
        loop {
            let current_kind = self.peek().clone();

            if current_kind == TokenKind::Eof {
                break;
            }

            match self.parse_decl() {
                Ok(decl) => decls.push(decl),
                Err(e) => return Err(e),
            }
        }
        let declarations = decls.into_iter().map(Stmt::Decl).collect();
        Ok(Program { declarations })
    }
}
