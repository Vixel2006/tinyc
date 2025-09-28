use crate::lexer::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Char,
    Void,
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Integer(i32),
    Float(f32),
    Bool(bool),
    Char(char),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    Div,

    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,

    LogicalAnd,
    LogicalOr,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,
    LogicalNot,
    BitwiseNot,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Value(Val),
    Identifier(Token),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Paren(Box<Expr>),
    Call(Token, Vec<Expr>),
    Assign(Token, Box<Expr>), // Assignment expression: identifier and assigned value
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub param_type: Type,
    pub identifier: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub var_type: Type,
    pub identifier: Token,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub return_type: Type,
    pub identifier: Token,
    pub params: Vec<Param>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decls {
    Var(VarDecl),
    Func(FuncDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Decl(Decls),
    Assign(Token, Expr),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
    While(Expr, Vec<Stmt>),
    Return(Option<Expr>),
    Expr(Expr),
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub declarations: Vec<Decls>,
}
