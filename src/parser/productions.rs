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
pub enum Expr<'a> {
    Value(Val),
    Identifier(&'a Token),
    Binary(Box<Expr<'a>>, BinaryOp, Box<Expr<'a>>),
    Unary(UnaryOp, Box<Expr<'a>>),
    Paren(Box<Expr<'a>>),
    Call(Token, Vec<Expr<'a>>),
    Assign(Token, Box<Expr<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param<'a> {
    pub param_type: Type,
    pub identifier: &'a Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl<'a> {
    pub var_type: Type,
    pub identifier: &'a Token,
    pub initializer: Option<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl<'a> {
    pub return_type: Type,
    pub identifier: &'a Token,
    pub params: Vec<Param<'a>>,
    pub body: Option<Vec<Stmt<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decls<'a> {
    Var(VarDecl<'a>),
    Func(FuncDecl<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    Decl(Decls<'a>),
    Assign(&'a Token, Expr<'a>),
    If(Expr<'a>, Vec<Stmt<'a>>, Option<Vec<Stmt<'a>>>),
    While(Expr<'a>, Vec<Stmt<'a>>),
    Return(Option<Expr<'a>>),
    Expr(Expr<'a>),
    Block(Vec<Stmt<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a> {
    pub declarations: Vec<Decls<'a>>,
}
