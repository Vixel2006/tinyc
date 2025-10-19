use super::productions::{
    Decls, Expr, FuncDecl, Param, Program, Stmt, Type, UnaryOp, Val, VarDecl,
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

    // pub fn parse_stmt(&mut self) -> Result<Stmt, String> {}

    pub fn parse_primary_expr(&mut self) -> Result<Expr, String> {
        if self.match_tokens(&[
            TokenKind::Integer,
            TokenKind::Decimal,
            TokenKind::Character,
            TokenKind::True,
            TokenKind::False,
        ]) {
            return self.parse_value();
        } else if self.check(TokenKind::Identifier) {
            return self.parse_identifier_expr();
        } else if self.check(TokenKind::LeftParen) {
            self.consume(); // Consume '('
            let expr = self.parse_expr()?; // Recursively parse the expression inside parentheses
            self.expect(TokenKind::RightParen, "Expected ')' after expression")?;
            return Ok(Expr::Paren(Box::new(expr)));
        } else {
            Err(format!(
                "Syntax Error: line {}, column {}. Expected identifier, literal, or '(' but found {}",
                self.tokens[self.curr].line,
                self.tokens[self.curr].column,
                self.tokens[self.curr].lexeme,
            ))
        }
    }

    pub fn parse_equality_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_comparison_expr()?;

        while self.match_tokens(&[TokenKind::IsEq, TokenKind::NotEq]) {
            let operator_token = self.tokens[self.curr].clone();
            self.consume();
            let right = self.parse_comparison_expr()?;
            let op = match operator_token.kind {
                TokenKind::IsEq => super::productions::BinaryOp::Equal,
                TokenKind::NotEq => super::productions::BinaryOp::NotEqual,
                _ => unreachable!(),
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    pub fn parse_comparison_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_additive_expr()?;

        while self.match_tokens(&[
            TokenKind::Greater,
            TokenKind::GEq,
            TokenKind::Less,
            TokenKind::LEq,
        ]) {
            let operator_token = self.tokens[self.curr].clone();
            self.consume();
            let right = self.parse_additive_expr()?;
            let op = match operator_token.kind {
                TokenKind::Greater => super::productions::BinaryOp::Greater,
                TokenKind::GEq => super::productions::BinaryOp::GreaterEqual,
                TokenKind::Less => super::productions::BinaryOp::Less,
                TokenKind::LEq => super::productions::BinaryOp::LessEqual,
                _ => unreachable!(),
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    pub fn parse_additive_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_multiplicative_expr()?;

        while self.match_tokens(&[TokenKind::Plus, TokenKind::Minus]) {
            let operator_token = self.tokens[self.curr].clone();
            self.consume();
            let right = self.parse_multiplicative_expr()?;
            let op = match operator_token.kind {
                TokenKind::Plus => super::productions::BinaryOp::Add,
                TokenKind::Minus => super::productions::BinaryOp::Sub,
                _ => unreachable!(),
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    pub fn parse_multiplicative_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_unary_expr()?;

        while self.match_tokens(&[TokenKind::Multiply, TokenKind::Div]) {
            let operator_token = self.tokens[self.curr].clone();
            self.consume();
            let right = self.parse_unary_expr()?;
            let op = match operator_token.kind {
                TokenKind::Multiply => super::productions::BinaryOp::Mult,
                TokenKind::Div => super::productions::BinaryOp::Div,
                _ => unreachable!(),
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    pub fn parse_unary_expr(&mut self) -> Result<Expr, String> {
        if self.match_tokens(&[
            TokenKind::Minus,
            TokenKind::LogicalNot,
            TokenKind::BitwiseNot,
        ]) {
            let operator_token = self.tokens[self.curr].clone();
            self.consume();
            let right = self.parse_unary_expr()?; // Unary operators are right-associative
            let op = match operator_token.kind {
                TokenKind::Minus => UnaryOp::Negate,
                TokenKind::LogicalNot => UnaryOp::LogicalNot,
                TokenKind::BitwiseNot => UnaryOp::BitwiseNot,
                _ => unreachable!(),
            };
            Ok(Expr::Unary(op, Box::new(right)))
        } else {
            self.parse_primary_expr()
        }
    }

    pub fn parse_value(&mut self) -> Result<Expr, String> {
        let tok = self.tokens[self.curr].clone();
        let val = match tok.kind {
            TokenKind::Integer => tok.lexeme.parse::<i32>().map(Val::Integer).map_err(|_| {
                format!(
                    "invalid integer literal '{}' at line {}",
                    tok.lexeme, tok.line
                )
            })?,
            TokenKind::Decimal => tok.lexeme.parse::<f32>().map(Val::Float).map_err(|_| {
                format!(
                    "invalid float literal '{}' at line {}",
                    tok.lexeme, tok.line
                )
            })?,
            TokenKind::Character => tok.lexeme.parse::<char>().map(Val::Char).map_err(|_| {
                format!("invalid char literal '{}' at line {}", tok.lexeme, tok.line)
            })?,
            TokenKind::True => Val::Bool(true),
            TokenKind::False => Val::Bool(false),
            _ => {
                return Err(format!(
                    "Unexpected token '{}' where a literal was expected at line {}",
                    tok.lexeme, tok.line
                ));
            }
        };

        self.consume();
        Ok(Expr::Value(val))
    }

    pub fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_equality_expr()
    }

    pub fn parse_identifier_expr(&mut self) -> Result<Expr, String> {
        let identifier_token = self.tokens[self.curr].clone();
        self.consume();
        Ok(Expr::Identifier(identifier_token))
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
                _ => Type::Unknown,
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
            let expr = self.parse_expr()?;
            Ok(VarDecl {
                var_type: ty,
                identifier: id,
                initializer: Some(expr),
            })
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
                _ => Type::Unknown,
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
