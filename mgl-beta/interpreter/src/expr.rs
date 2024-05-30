use crate::scanner::Token;

enum Expr {
    
}

struct Binary {
    left: Expr,
    op: Token,
    right: Expr,
};

impl Binary {
    fn new(left: Expr, op: Token, right: Expr) -> Self {
        Self {
            left,
            op,
            right,
        }
    }
}