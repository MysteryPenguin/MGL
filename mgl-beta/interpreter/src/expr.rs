use crate::scanner::Token;

enum Expr {
    Binary(Box<Binary>),
    // Other expressions...
}

struct Binary {
    left: Box<Expr>,
    operator: Token,
    right: Box<Expr>,
}

impl Binary {
    fn new(left: Box<Expr>, operator: Token, right: Box<Expr>) -> Self {
        Self { left, operator, right }
    }
}