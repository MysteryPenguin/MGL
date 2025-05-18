use crate::File;
use crate::saving::expr::ClosureDecl;
use crate::saving::r#type::{FnType, LiteralType, Type};

use crate::saving::{
    error::{Error, ErrorBuilder, ErrorType},
    expr::Expr,
    stmt::{ClassDecl, Decl, FnDecl, Stmt, VarDecl},
    symbol::{SourceLocation, Symbol},
    token::Token,
    token_type::TokenType,
    value::{Literal, Value},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    error_builder: ErrorBuilder,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, file: File) -> Self {
        Self {
            tokens,
            current: 0,
            error_builder: ErrorBuilder(file),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Decl>, Error> {
        let mut decls: Vec<Decl> = Vec::new();

        while !self.is_at_end() {
            let decl = self.declaration_global()?;
            decls.push(decl);
        }

        Ok(decls)
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        return self.assignment();
    }

    fn declaration_global(&mut self) -> Result<Decl, Error> {
        if self.match_tokens(&[TokenType::Pub]) {
            let decl = self.declaration_global()?;

            match decl {
                Decl::Class(class_decl) => return Ok(Decl::Pub(Box::new(Decl::Class(class_decl)))),
                Decl::Fn(fn_decl) => return Ok(Decl::Pub(Box::new(Decl::Fn(fn_decl)))),
                Decl::Var(var_decl) => return Ok(Decl::Pub(Box::new(Decl::Var(var_decl)))),
                Decl::Pub(_) => {
                    return Err(self.error_builder.build(
                        ErrorType::Syntax,
                        String::from("a public declaration is not allowed in a public declaration"),
                        self.peek().to_source_location(),
                    ));
                }
            }
        }
        if self.match_tokens(&[TokenType::Class]) {
            return Ok(Decl::Class(self.class_decl()?));
        }
        if self.match_tokens(&[TokenType::Fn]) {
            return Ok(Decl::Fn(self.function("function".to_owned())?));
        }
        if self.match_tokens(&[TokenType::Let]) {
            return Ok(Decl::Var(self.var_declaration()?));
        }

        Err(self.error_builder.build(
            ErrorType::Syntax,
            String::from("statements cannot be executed in the global scope"),
            self.peek().to_source_location(),
        ))
    }

    fn declaration(&mut self) -> Result<Stmt, Error> {
        if self.match_tokens(&[TokenType::Fn]) {
            return Ok(Stmt::FnDecl(self.function("function".to_owned())?));
        }
        if self.match_tokens(&[TokenType::Let]) {
            return Ok(Stmt::Var(self.var_declaration()?));
        }
        if self.match_tokens(&[TokenType::Class]) {
            return Err(self.error_builder.build(
                ErrorType::Syntax,
                String::from("class declarations are not allowed in a block statement"),
                self.peek().to_source_location(),
            ));
        }
        if self.match_tokens(&[TokenType::Pub]) {
            return Err(self.error_builder.build(
                ErrorType::Syntax,
                String::from("public declarations are not allowed in a block statement"),
                self.peek().to_source_location(),
            ));
        }

        self.stmt()
    }

    fn function(&mut self, kind: String) -> Result<FnDecl, Error> {
        let name = self
            .consume(
                TokenType::Identifier,
                format!("expect identifier after {kind} name"),
            )?
            .to_symbol();
        self.consume(
            TokenType::LeftParen,
            format!("expect '(' after {kind} name"),
        )?;

        let mut params: Vec<(Symbol, Type)> = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    let token = self.peek();
                    return Err(self.error_builder.build(
                        ErrorType::Overflow,
                        format!("function '{name}' cannot have more than 255 parameters"),
                        SourceLocation {
                            line: token.line,
                            col: token.col,
                        },
                    ));
                }
                let ident = self
                    .consume(TokenType::Identifier, String::from("expect parameter name"))?
                    .to_symbol();
                self.consume(
                    TokenType::Colon,
                    String::from(
                        "expect ':' because type anotation is required in function parameters",
                    ),
                )?;
                let r#type = self.match_type()?;

                params.push((ident, r#type));

                if !self.match_tokens(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        self.consume(
            TokenType::RightParen,
            "expect ')' after parameters".to_string(),
        )?;

        self.consume(
            TokenType::LeftBrace,
            format!("expect '{{' before {kind} body"),
        )?;
        let body = self.block()?;

        Ok(FnDecl { name, params, body })
    }

    fn stmt(&mut self) -> Result<Stmt, Error> {
        if self.match_tokens(&[TokenType::For]) {
            return self.for_stmt();
        }
        if self.match_tokens(&[TokenType::If]) {
            return self.if_stmt();
        }
        if self.match_tokens(&[TokenType::Print]) {
            return self.print_stmt();
        }
        if self.match_tokens(&[TokenType::Return]) {
            return self.return_stmt();
        }
        if self.match_tokens(&[TokenType::While]) {
            return self.while_stmt();
        }
        if self.match_tokens(&[TokenType::LeftBrace]) {
            return Ok(Stmt::Block(self.block()?));
        }
        if self.match_tokens(&[TokenType::Import]) {
            return self.import_stmt();
        }

        self.expression_stmt()
    }

    fn import_stmt(&mut self) -> Result<Stmt, Error> {
        let mut imports: Vec<Symbol> = Vec::new();

        if !self.match_tokens(&[TokenType::From]) {
            loop {
                imports.push(
                    self.consume(
                        TokenType::Identifier,
                        String::from("expect 'import' identifier"),
                    )?
                    .to_symbol(),
                );

                if !self.match_tokens(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(
            TokenType::From,
            String::from("expect 'from' before file path"),
        )?;

        let file_path_token = self.consume(
            TokenType::String,
            String::from("expect file path after 'from'"),
        )?;
        let file_path = Symbol::new(
            file_path_token.literal.unwrap().to_string().unwrap(),
            file_path_token.line,
            file_path_token.col,
        );

        self.semicolon();

        Ok(Stmt::Import { imports, file_path })
    }

    fn class_decl(&mut self) -> Result<ClassDecl, Error> {
        let name = self
            .consume(TokenType::Identifier, "expect class name".to_string())?
            .to_symbol();
        self.consume(
            TokenType::LeftBrace,
            "expect '{' before class body".to_string(),
        )?;

        let mut methods = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.function(String::from("method"))?);
        }

        self.consume(
            TokenType::RightBrace,
            "expect '}' after class body".to_string(),
        )?;

        Ok(ClassDecl { name, methods })
    }

    fn for_stmt(&mut self) -> Result<Stmt, Error> {
        self.consume(TokenType::LeftParen, "expect '(' after 'for'".to_string())?;

        let initializer = if self.match_tokens(&[TokenType::Semicolon]) {
            None
        } else if self.match_tokens(&[TokenType::Let]) {
            Some(Stmt::Var(self.var_declaration()?))
        } else {
            Some(self.expression_stmt()?)
        };

        let condition = if !self.check(&TokenType::Semicolon) {
            self.expression()?
        } else {
            let token = self.peek();
            Expr::Literal {
                lit: Value::Literal(Literal::Bool(true)),
                loc: SourceLocation {
                    line: token.line,
                    col: token.col,
                },
            }
        };
        self.consume(
            TokenType::Semicolon,
            "expect ';' after loop condition".to_string(),
        )?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(
            TokenType::RightParen,
            "expected ')' after for clauses".to_string(),
        )?;
        let mut body = self.stmt()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expr(increment)]);
        }
        body = Stmt::While {
            condition,
            body: Box::new(body),
        };

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn while_stmt(&mut self) -> Result<Stmt, Error> {
        let condition = self.expression()?;
        
        let body = self.stmt()?;

        Ok(Stmt::While {
            condition,
            body: Box::new(body),
        })
    }

    fn if_stmt(&mut self) -> Result<Stmt, Error> {
        let condition = self.expression()?;

        let then_branch = Box::new(self.stmt()?);
        let else_branch = if self.match_tokens(&[TokenType::Else]) {
            Some(Box::new(self.stmt()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn print_stmt(&mut self) -> Result<Stmt, Error> {
        let value = self.expression()?;
        self.semicolon();

        Ok(Stmt::Print(value))
    }

    fn return_stmt(&mut self) -> Result<Stmt, Error> {
        let loc = self.previous().to_source_location();

        let value = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.semicolon();
        Ok(Stmt::Return { loc, value })
    }

    fn expression_stmt(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        self.semicolon();

        Ok(Stmt::Expr(expr))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut stmts: Vec<Stmt> = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        self.consume(
            TokenType::RightBrace,
            String::from("expected '}' after block"),
        )?;

        Ok(stmts)
    }

    fn match_type(&mut self) -> Result<Type, Error> {
        let token = self.peek().clone();
        self.advance();
        match token.token_type {
            TokenType::Identifier => Ok(token.to_symbol().to_type()),
            TokenType::LeftParen => Ok(Type::Literal(Some(LiteralType::Fn(self.function_type()?)))),
            _ => Err(self.error_builder.build(
                ErrorType::Syntax,
                String::from("expected type after type declaration"),
                token.to_source_location(),
            )),
        }
    }

    fn var_declaration(&mut self) -> Result<VarDecl, Error> {
        let sym = self
            .consume(
                TokenType::Identifier,
                String::from("expected variable name"),
            )?
            .to_symbol();

        let mut init = None;
        let mut r#type = None;

        if self.match_tokens(&[TokenType::Colon]) {
            r#type = Some(self.match_type()?);
        }
        if self.match_tokens(&[TokenType::Equal]) {
            init = Some(self.expression()?);
        }

        self.semicolon();

        Ok(VarDecl { sym, init, r#type })
    }

    fn function_type(&mut self) -> Result<FnType, Error> {
        let mut params = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    let token = self.peek();
                    return Err(self.error_builder.build(
                        ErrorType::Overflow,
                        String::from("closure cannot have more than 255 parameters"),
                        SourceLocation {
                            line: token.line,
                            col: token.col,
                        },
                    ));
                }
                let r#type = self.match_type()?;

                params.push(r#type);

                if !self.match_tokens(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(
            TokenType::RightParen,
            String::from("expect ')' after parameters"),
        )?;

        self.consume(
            TokenType::Arrow,
            String::from("expected '->' after parameters for function type"),
        )?;

        let return_type = Box::new(self.match_type()?);

        Ok(FnType {
            params,
            return_type,
        })
    }

    fn assignment(&mut self) -> Result<Expr, Error> {
        let expr = self.or()?;

        if self.match_tokens(&[TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;

            return match &expr {
                Expr::Var(sym) => Ok(Expr::Assign {
                    sym: sym.clone(),
                    value: Box::new(value),
                }),
                Expr::Get { attr, lhs } => Ok(Expr::Set {
                    lhs: lhs.clone(),
                    attr: attr.clone(),
                    rhs: Box::new(value),
                }),

                _ => {
                    let token = self.peek();
                    Err(self.error_builder.build(
                        ErrorType::Syntax,
                        format!("invalid assignment target '{}'", equals.lexeme),
                        SourceLocation {
                            line: token.line,
                            col: token.col,
                        },
                    ))
                }
            };
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.and()?;

        while self.match_tokens(&[TokenType::Or]) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;
        while self.match_tokens(&[TokenType::And]) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;

        while self.match_tokens(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator_token = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary {
                right: Box::new(right),
                operator: operator_token,
                left: Box::new(expr),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.term()?;

        while self.match_tokens(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator_token = self.previous();
            let right = self.term()?;
            expr = Expr::Binary {
                right: Box::new(right),
                operator: operator_token,
                left: Box::new(expr),
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;
        while self.match_tokens(&[TokenType::Minus, TokenType::Plus]) {
            let operator_token = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary {
                right: Box::new(right),
                operator: operator_token,
                left: Box::new(expr),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;

        while self.match_tokens(&[TokenType::Slash, TokenType::Star]) {
            let operator_token = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary {
                right: Box::new(right),
                operator: operator_token,
                left: Box::new(expr),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if self.match_tokens(&[TokenType::Bang, TokenType::Minus]) {
            let operator_token = self.previous();
            let expr = self.unary()?;
            return Ok(Expr::Unary {
                expr: Box::new(expr),
                operator: operator_token,
            });
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr, Error> {
        let mut expr = self.primary()?;

        loop {
            if self.match_tokens(&[TokenType::LeftParen]) {
                return self.finish_call(Box::new(expr));
            } else if self.match_tokens(&[TokenType::Dot]) {
                let attr = self
                    .consume(
                        TokenType::Identifier,
                        String::from("expected property name after '.'"),
                    )?
                    .to_symbol();
                expr = Expr::Get {
                    attr,
                    lhs: Box::new(expr),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn closure(&mut self) -> Result<ClosureDecl, Error> {
        let mut params: Vec<(Symbol, Type)> = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    let token = self.peek();
                    return Err(self.error_builder.build(
                        ErrorType::Overflow,
                        String::from("closure cannot have more than 255 parameters"),
                        SourceLocation {
                            line: token.line,
                            col: token.col,
                        },
                    ));
                }
                let ident = self
                    .consume(TokenType::Identifier, String::from("expect parameter name"))?
                    .to_symbol();
                self.consume(
                    TokenType::Colon,
                    String::from(
                        "expect ':' because type anotation is required in function parameters",
                    ),
                )?;
                let r#type = self.match_type()?;

                params.push((ident, r#type));

                if !self.match_tokens(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        self.consume(
            TokenType::RightParen,
            String::from("expect ')' after parameters"),
        )?;

        self.consume(TokenType::Arrow, String::from("expect '->' after ')'"))?;

        let body = Box::new(self.stmt()?);

        Ok(ClosureDecl { params, body })
    }

    fn finish_call(&mut self, callee: Box<Expr>) -> Result<Expr, Error> {
        let mut args: Vec<Box<Expr>> = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if args.len() >= 255 {
                    let token = self.peek();
                    return Err(self.error_builder.build(
                        ErrorType::Overflow,
                        String::from("function cannot have more than 255 parameters"),
                        SourceLocation {
                            line: token.line,
                            col: token.col,
                        },
                    ));
                }
                args.push(Box::new(self.expression()?));

                if !self.match_tokens(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let loc = self
            .consume(
                TokenType::RightParen,
                String::from("expect ')' after arguments"),
            )?
            .to_source_location();

        Ok(Expr::Call { callee, loc, args })
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        if self.match_tokens(&[TokenType::False]) {
            let token = self.peek();
            return Ok(Expr::Literal {
                lit: Value::Literal(Literal::Bool(false)),
                loc: SourceLocation {
                    line: token.line,
                    col: token.col,
                },
            });
        }
        if self.match_tokens(&[TokenType::True]) {
            let token = self.peek();
            return Ok(Expr::Literal {
                lit: Value::Literal(Literal::Bool(true)),
                loc: SourceLocation {
                    line: token.line,
                    col: token.col,
                },
            });
        }
        if self.match_tokens(&[TokenType::None]) {
            let token = self.peek();
            return Ok(Expr::Literal {
                lit: Value::Literal(Literal::None),
                loc: SourceLocation {
                    line: token.line,
                    col: token.col,
                },
            });
        }

        if self.match_tokens(&[TokenType::Number, TokenType::String]) {
            match self.previous().literal {
                Some(lit) => {
                    let previous = self.previous();
                    return Ok(Expr::Literal {
                        lit,
                        loc: SourceLocation {
                            line: previous.line,
                            col: previous.col,
                        },
                    });
                }
                None => {
                    let token = self.peek();
                    return Err(self.error_builder.build(
                        ErrorType::Syntax,
                        format!("expected string or number '{}'", token.lexeme),
                        SourceLocation {
                            line: token.line,
                            col: token.col,
                        },
                    ));
                }
            }
        }

        if self.match_tokens(&[TokenType::This]) {
            return Ok(Expr::This(self.previous().to_source_location()));
        }

        if self.match_tokens(&[TokenType::Identifier]) {
            return Ok(Expr::Var(self.previous().to_symbol()));
        }

        if self.match_tokens(&[TokenType::LeftParen]) {
            let start = self.current;
            match self.closure() {
                Ok(decl) => {
                    return Ok(Expr::Closure {
                        decl,
                        loc: self.peek().to_source_location(),
                    });
                }
                Err(err) => {
                    self.current = start;
                    let expr = self.expression()?;
                    self.consume(
                        TokenType::RightParen,
                        String::from("expect ')' after expression"),
                    )?;
                    if self.match_tokens(&[TokenType::Arrow]) {
                        return Err(err);
                    }
                    return Ok(Expr::Group(Box::new(expr)));
                }
            }
        }

        let token = self.peek();
        Err(self.error_builder.build(
            ErrorType::Syntax,
            format!("Expected Expression '{}'", self.peek().lexeme),
            SourceLocation {
                line: token.line,
                col: token.col,
            },
        ))
    }

    fn consume(&mut self, token_type: TokenType, message: String) -> Result<Token, Error> {
        if self.check(&token_type) {
            return Ok(self.advance());
        }

        let token = self.previous();

        Err(self.error_builder.build(
            ErrorType::Syntax,
            message,
            SourceLocation {
                line: token.line,
                col: token.col,
            },
        ))
    }

    fn match_tokens(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(&token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().token_type == *token_type
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        return self.previous().clone();
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn semicolon(&mut self) {
        if self.check(&TokenType::Semicolon) {
            self.advance();
        }
    }
}
