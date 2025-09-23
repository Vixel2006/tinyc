use super::lexer::token::Token;

pub enum Expr {
    Literal { id: Token },
    VarRef { id: Token },
    Assigment { id: Token, assignee: Box<Expr> },
    BinaryOp { left: Box<Expr>, op: Token, right: Box<Expr> },
    UnaryOp { op: Token, Operand: Box<Expr> },
    Grouping { expr: Box<Expr> },
    FuncCall { id: Token, Box<Expr>},
    Nil,
}


pub enum Decls {
    VarDecl { type: Token, id: Token, op: Token, literal: Token },
    FuncDecl { type: Token, id: Token, params: Box<Expr> },
}

pub enum Stmts {
    ExprStmt { assigner: Token, eq: Token, left: Box<Expr>, op: Token, right: Box<Expr> },
    IfStmt,
    WhileStmt,
    ReturnStmt { ret: Token, id: Token },
    Block,
}

pub enum ASTSymbols {
    Decl,
    Stmt,
    Expr,
}

pub struct Goal {
    pub sentences: Vec<ASTSymbols>,
}

