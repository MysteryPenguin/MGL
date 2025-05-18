use std::collections::HashMap;

use crate::{saving::{error::*, symbol::SourceLocation, token::Token, token_type::TokenType, value::*}, File};

pub struct Lexer {
    file: File,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    keywords: HashMap<String, TokenType>,
    error_builder: ErrorBuilder,
    col: usize,
}

impl Lexer {
    pub fn new(file: File) -> Self {
        Self {
            file: file.clone(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            keywords: HashMap::from([
                ("and".to_string(), TokenType::And),
                ("class".to_string(), TokenType::Class),
                ("else".to_string(), TokenType::Else),
                ("false".to_string(), TokenType::False),
                ("for".to_string(), TokenType::For),
                ("fn".to_string(), TokenType::Fn),
                ("if".to_string(), TokenType::If),
                ("none".to_string(), TokenType::None),
                ("or".to_string(), TokenType::Or),
                ("print".to_string(), TokenType::Print),
                ("return".to_string(), TokenType::Return),
                ("super".to_string(), TokenType::Super),
                ("this".to_string(), TokenType::This),
                ("true".to_string(), TokenType::True),
                ("let".to_string(), TokenType::Let),
                ("while".to_string(), TokenType::While),
                ("import".to_string(), TokenType::Import),
                ("from".to_string(), TokenType::From),
                ("pub".to_string(), TokenType::Pub)
            ]),
            error_builder: ErrorBuilder(file),
            col: 0
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, Error> {
        while !self.is_at_end() {
            self.start = self.current;

            match self.scan_token() {
                Ok(_) => {},
                Err(err) => return Err(err)
            }
        }

        self.tokens.push(Token::new(TokenType::EOF, String::new(), None, self.line, self.current));

        return Ok(self.tokens.clone());
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.file.source.len()
    }

    fn scan_token(&mut self) -> Result<(), Error> {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => {
                if self.matches_char('>') {
                    self.add_token(TokenType::Arrow)
                } else {
                    self.add_token(TokenType::Minus)
                }
            },
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                if self.matches_char('=') {
                    self.add_token(TokenType::BangEqual)
                } else {
                    self.add_token(TokenType::Bang)
                }
            },
            '=' => {
                if self.matches_char('=') {
                    self.add_token(TokenType::EqualEqual)
                } else {
                    self.add_token(TokenType::Equal)
                }
            },
            '<' => {
                if self.matches_char('=') {
                    self.add_token(TokenType::LessEqual)
                } else {
                    self.add_token(TokenType::Less)
                }
            },
            '>' => {
                if self.matches_char('=') {
                    self.add_token(TokenType::GreaterEqual)
                } else {
                    self.add_token(TokenType::Greater)
                }
            },
            '/' => {
                if self.matches_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash)
                }
            },
            ' ' | '\r' | '\t' => {},
            '\n' => {
                self.line += 1;
                self.col = 0;
            },
            '"' => self.string()?,
            ':' => self.add_token(TokenType::Colon),
            _ => {
                if self.is_digit(c) {
                    self.number()?;
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    return Err(self.error_builder.build(ErrorType::Syntax, format!("unexpected character '{c}'"), SourceLocation { line: self.line, col: self.col }));
                }
            }
        }

        Ok(())
    }

    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn identifier(&mut self) {
        while self.is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text = self.file.source[self.start..self.current].to_string();

        let keywords = &self.keywords;

        match keywords.get(&text) {
            Some(token_type) => {
                self.add_token(token_type.clone());
            },
            None => {
                self.add_token(TokenType::Identifier);
            }
        }
    }

    fn number(&mut self) -> Result<(), Error> {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.advance();
            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        match self.file.source[self.start..self.current].parse() {
            Ok(number) => {
                self.add_token_lit(TokenType::Number, Some(Value::Literal(Literal::Number(number))));
                Ok(())
            },
            Err(err) => Err(self.error_builder.build(ErrorType::Syntax, err.to_string(), SourceLocation { line: self.line, col: self.col }))
        }
    }

    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn string(&mut self) -> Result<(), Error> {
        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(self.error_builder.build(ErrorType::Syntax, String::from("unterminated string"), SourceLocation { line: self.line, col: self.col }))
        }

        self.advance();

        let value = self.file.source[self.start + 1..self.current - 1].to_string();
        self.add_token_lit(TokenType::String, Some(Value::Literal(Literal::String(value))));

        Ok(())
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0' 
        }

        return self.file.source.as_bytes()[self.current] as char;
    }

    fn peek_next(&self) -> char {
        if (self.current + 1) >= self.file.source.len() {
            return '\0'
        }

        return self.file.source.as_bytes()[self.current + 1] as char;
    }

    fn advance(&mut self) -> char {
        let bytes = self.file.source.as_bytes();
        self.current += 1;
        self.col += 1;

        return bytes[self.current - 1] as char;
    }

    fn matches_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.file.source.as_bytes()[self.current] as char != expected {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn add_token(&mut self, token: TokenType) {
        self.add_token_lit(token, None);
    }

    fn add_token_lit(&mut self, token: TokenType, literal: Option<Value>) {
        let text = self.file.source[self.start..self.current].to_string();

        self.tokens.push(Token::new(token, text, literal, self.line, self.col));
    }
}