use super::{stmt::Stmt, symbol::{SourceLocation, Symbol}, token::Token, r#type::Type, value::Value};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>
    },
    Call { callee: Box<Expr>, loc: SourceLocation, args: Vec<Box<Expr>> },
    Group(Box<Expr>),
    Literal { lit: Value, loc: SourceLocation },
    Unary {
        operator: Token,
        expr: Box<Expr>
    },
    Var(Symbol),
    Assign { sym: Symbol, value: Box<Expr> },
    Get { attr: Symbol, lhs: Box<Expr> },
    Set { lhs: Box<Expr>, attr: Symbol, rhs: Box<Expr> },
    This(SourceLocation),
    Closure { decl: ClosureDecl, loc: SourceLocation }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureDecl {
    pub params: Vec<(Symbol, Type)>,
    pub body: Box<Stmt>
}