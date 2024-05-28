use std::collections::HashMap;

static mut KEYWORDS: HashMap<String, TokenType> = HashMap::new();

fn insert(key: String, value: TokenType) {
    unsafe {
        KEYWORDS.insert(key, value);
    }
}

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            source: String::from(source),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, String> {
        let mut errors: Vec<String> = Vec::new();
        while !self.is_at_end() {
            self.start = self.current;
            match self.scan_token() {
                Ok(_) => (),
                Err(msg) => errors.push(msg),
            }
        }

        self.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: String::new(),
            literal: None,
            line: self.line,
        });

        if errors.len() > 0 {
            let mut joined = String::new();
            for msg in &errors {
                joined.push_str(msg);
                joined.push_str("\n");
            }
            return Err(joined);
        }
        Ok(self.tokens.clone())
    }

    fn is_at_end(self: &Self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) -> Result<Token, String> {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            ';' => self.add_token(TokenType::Semicolon),
            '+' => self.add_token(TokenType::Plus),
            '-' => self.add_token(TokenType::Minus),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                let token = if self.matches_char('=') {
                    // !=
                    TokenType::BangEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(token);
            }
            '=' => {
                let token = if self.matches_char('=') {
                    // !=
                    TokenType::EqualEqual
                } else {
                    TokenType::LessEqual
                };
                self.add_token(token);
            }
            '>' => {
                let token = if self.matches_char('=') {
                    // !=
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(token);
            }
            '<' => {
                let token = if self.matches_char('=') {
                    // !=
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(token);
            }
            '/' => {
                if self.matches_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => (),
            '\n' => {
                self.line += 1;
            }
            '"' => self.string()?,
            'o' => {
                if self.matches_char('r') {
                    self.add_token(TokenType::Or);
                }
            }
            _ => {
                if self.is_digit(c) {
                    self.number()
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    return Err(format!("Unexpected character at line {}: {}", self.line, c));
                }
            }
        }

        todo!()
    }

    fn is_alpa_num(&self, c: char) -> bool {
        return self.is_alpha(c) || self.is_digit(c);
    }

    fn is_alpha(&self, c: char) -> bool {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
    }

    fn identifier(&mut self) {
        let peek = self.peek();
        while self.is_alpa_num(peek) {
            self.advance();
        }

        self.add_token(TokenType::Identifier);
    }

    fn peek_next(&self) -> char {
        let c = self.source.as_bytes()[self.current + 1];
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        c as char
    }

    fn is_digit(&self, c: char) -> bool {
        return c >= '0' && c <= '9';
    }

    fn number(self: &mut Self) {
        let peek = self.peek();
        let mut float: String = String::new();
        while self.is_digit(peek) {
            self.advance();
        }
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.advance();

            while self.is_digit(peek) {
                self.advance();
            }
        }

        for i in self.start..self.current {
            float.push(self.source.as_bytes()[i] as char);
        }

        self.add_token_lit(TokenType::Number, Some(LiteralValue::FloatValue(float.parse().unwrap())));
    }

    fn peek(&mut self) -> char {
        let chars: Vec<char> = self.source.chars().collect();

        if !self.is_at_end() {
            return '\0';
        }

        chars[self.current]
    }

    fn string(&mut self) -> Result<(), String> {
        let mut value = String::new();

        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(format!("Unterminated string at line {}.", self.line));
        }

        self.advance();

        for i in self.start + 1..self.current - 1 {
            value.push(self.source.as_bytes()[i] as char);
        }

        self.add_token_lit(TokenType::String, Some(LiteralValue::StringValue(value)));
        Ok(())
    }

    fn matches_char(&mut self, expected: char) -> bool {
        let chars: Vec<char> = self.source.chars().collect();

        if self.is_at_end() {
            return false;
        }

        if chars[self.current] != expected {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn advance(&mut self) -> char {
        let c = self.source.as_bytes()[self.current];
        self.current += 1;

        c as char
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_lit(token_type, None);
    }

    fn add_token_lit(self: &mut Self, token_type: TokenType, literal: Option<LiteralValue>) {
        let mut text = String::new();
        for i in self.start..self.current {
            text.push(self.source.as_bytes()[i] as char);
        }

        self.tokens.push(Token {
            token_type,
            lexeme: text,
            literal,
            line: self.line,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<LiteralValue>,
    line: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Option<LiteralValue>,
        line: usize,
    ) -> Self {
        Self {
            token_type,
            lexeme,
            literal,
            line,
        }
    }

    pub fn to_string(&self) -> String {
        format!("{} {} {:?}", self.token_type, self.lexeme, self.literal)
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    // Sigle-char tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // 1 or 2 chars
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fn,
    For,
    If,
    None,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Let,
    While,

    Eof,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    IntValue(i64),
    StringValue(String),
    FloatValue(f64),
    IdentifierValue(String),
}
