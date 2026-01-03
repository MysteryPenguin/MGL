use crate::{
    error::{ErrorType, MGLError}, loc::SourceLoc, saving::{expr::Operator, literal::Literal}
};

use super::{symbol::*, token_type::TokenType};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Box<str>,
    pub literal: Option<Literal>,
    pub line: usize,
    pub col: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: &str,
        literal: Option<Literal>,
        line: usize,
        col: usize,
    ) -> Self {
        Self {
            token_type,
            lexeme: lexeme.into(),
            literal,
            line,
            col,
        }
    }
}

impl Into<SourceLoc> for &Token {
    fn into(self) -> SourceLoc {
        SourceLoc {
            line: self.line,
            col: self.col,
        }
    }
}

impl Into<Symbol> for &Token {
    fn into(self) -> Symbol {
        Symbol {
            name: self.lexeme.clone(),
            line: self.line,
            col: self.col,
        }
    }
}

impl Into<Oper

#[derive(Debug, Clone, PartialEq)]
pub struct TokenStream {
    tokens: Vec<Token>,
    current: usize,
}

impl TokenStream {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            current: 0,
        }
    }

    pub fn consume(&mut self, token_type: TokenType, msg: String) -> Result<&Token, MGLError> {
        if self.check(&token_type) {
            return Ok(self.advance());
        }

        let token = self.peek();

        return Err(MGLError {
            error_type: ErrorType::SyntaxError,
            msg,
            loc: Some(SourceLoc {
                line: token.line,
                col: token.col,
            }),
        });
    }

    pub fn match_tokens(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    pub fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().token_type == *token_type
    }

    pub fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    pub fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    pub fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    pub fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}

impl<T: From<TokenType>> Into<Operator<T>> for &Token {
    fn into(self) -> Operator<T> {
        Operator {
            loc: SourceLoc {
                line: self.line,
                col: self.col,
            },
            operator: self.token_type.into(),
        }
    }
}
