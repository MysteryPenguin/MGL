use crate::saving::expr::{BinOp, UnaryOp};

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Colon,

    // Logical Operators
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,

    // Special Operators
    Arrow,

    // Literals
    Ident,
    String,
    Number,

    // Keywords
    Class,
    Struct,
    Fn,
    Let,
    Const,
    SelfTy,
    SelfParam,
    While,
    For,
    If,
    Else,
    Return,
    True,
    False,
    Pub,

    EOF,
}

impl Into<BinOp> for TokenType {
    fn into(self) -> BinOp {
        match self {
            TokenType::Plus => BinOp::Plus,
            TokenType::Minus => BinOp::Minus,
            TokenType::Star => BinOp::Mul,
            TokenType::Slash => BinOp::Div,

            TokenType::EqualEqual => BinOp::EqEq,
            TokenType::BangEqual => BinOp::BangEq,

            TokenType::GreaterEqual => BinOp::GreaterEq,
            TokenType::LessEqual => BinOp::LessEq,

            TokenType::Greater => BinOp::Greater,
            TokenType::Less => BinOp::Less,
            _ => panic!("You don't checked the operator before!"),
        }
    }
}

impl Into<UnaryOp> for TokenType {
    fn into(self) -> UnaryOp {
        match self {
            TokenType::Bang => UnaryOp::Bang,

            TokenType::Plus => UnaryOp::Plus,
            TokenType::Minus => UnaryOp::Minus,
            _ => panic!("You don't checked the operator before!"),
        }
    }
}
