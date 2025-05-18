use super::{symbol::*, token_type::TokenType, value::Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Value>,
    pub line: usize,
    pub col: usize
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, literal: Option<Value>, line: usize, col: usize) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
            line,
            col
        }
    }

    pub fn to_symbol(&self) -> Symbol {
        Symbol::new(self.lexeme.clone(), self.line, self.col)
    }

    pub fn to_source_location(&self) -> SourceLocation {
        SourceLocation { line: self.line, col: self.col }
    }
}