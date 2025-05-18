use std::fmt;

use super::{expr::Expr, symbol::{SourceLocation, Symbol}, r#type::Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Return { loc: SourceLocation, value: Option<Expr> },
    Var(VarDecl),
    Block(Vec<Stmt>),
    If { condition: Expr, then_branch: Box<Stmt>, else_branch: Option<Box<Stmt>> },
    While { condition: Expr, body: Box<Stmt> },
    FnDecl(FnDecl),
    Import { imports: Vec<Symbol>, file_path: Symbol }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(_) => write!(f, "expression"),
            Self::Print(_) => write!(f, "print"),
            Self::Return { loc: _, value: _ } => write!(f, "return"),
            Self::Var(_) => write!(f, "var"),
            Self::Block(_) => write!(f, "block"),
            Self::If { condition: _, then_branch: _, else_branch: _ } => write!(f, "if"),
            Self::While { condition: _, body: _ } => write!(f, "while"),
            Self::FnDecl(_) => write!(f, "function"),
            Self::Import { imports: _, file_path: _ } => write!(f, "import"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Fn(FnDecl),
    Class(ClassDecl),
    Var(VarDecl),
    Pub(Box<Decl>)
}

impl Decl {
    pub fn get_name(&self) -> &Symbol {
        match self {
            Self::Fn(FnDecl { name, params: _, body: _ }) => name,
            Self::Class(ClassDecl { name, methods: _ }) => name,
            Self::Var(VarDecl { sym, init: _, r#type: _ }) => sym,
            Self::Pub(decl) => decl.get_name()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub sym: Symbol,
    pub init: Option<Expr>,
    pub r#type: Option<Type>
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    pub name: Symbol,
    pub params: Vec<(Symbol, Type)>,
    pub body: Vec<Stmt>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    pub name: Symbol,
    pub methods: Vec<FnDecl>
}