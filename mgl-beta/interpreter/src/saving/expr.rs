use crate::{
    error::{ErrorType, MGLError}, interpret::Interpret, loc::SourceLoc, parse::Parse, saving::{literal::Literal, pattern::Pattern, token::TokenStream, token_type::TokenType}
};

use super::{stmt::Stmt, symbol::Symbol, r#type::Type};

/// The expressions of MGL.
/// An expression is everything that returns something.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A binary expression is an expression that needs 2 expression and one operator.
    /// # Schema
    /// ```
    /// <left: expr> <operator: + - * / % && ||> <right: expr>
    /// ```
    /// # Examples
    /// ```
    /// 1 + 1
    /// 1 - 1
    /// 1 * 1
    /// 1 / 1
    /// 1 % 1
    ///
    /// true and true
    /// true or false
    /// ```
    Binary {
        /// The expression on the left-hand-side.
        left: Box<Expr>,
        /// The operator `+ - * / % && ||`.
        operator: Operator<BinOp>,
        /// The expression on the right-hand-side.
        right: Box<Expr>,
    },
    /// A call expression has to be called with another expression.
    /// # Schema
    /// ```
    /// <callee: expr> <arg: expr>
    /// ```
    /// # Examples
    /// ```
    /// function 1
    /// function(1, 2)
    /// function()
    /// ```
    Call {
        callee: Box<Expr>,
        loc: SourceLoc,
        arg: Box<Expr>,
    },
    Group(Box<Expr>),
    Literal {
        lit: Literal,
        loc: SourceLoc,
    },
    Unary {
        operator: Operator<UnaryOp>,
        expr: Box<Expr>,
    },
    Var(Symbol),
    Assign {
        sym: Symbol,
        value: Box<Expr>,
    },
    Get {
        attr: Symbol,
        lhs: Box<Expr>,
    },
    Set {
        lhs: Box<Expr>,
        attr: Symbol,
        rhs: Box<Expr>,
    },
    This(SourceLoc),
    Closure {
        decl: ClosureDecl,
        loc: SourceLoc,
    },
    List {
        loc: SourceLoc,
        exprs: Vec<Expr>,
    },
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
    Block(Vec<Stmt>),
    If(IfExpr),
    While(WhileExpr),
    Return {
        loc: SourceLoc,
        value: Box<Expr>,
    },
    LocalVar {
        name: Pattern,
        r#type: Option<Type>,
        value: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Operator<T> {
    pub operator: T,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,

    EqEq,
    BangEq,

    GreaterEq,
    LessEq,

    Greater,
    Less,

    Or,
    And,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Plus,
    Minus,

    Bang,
}

impl Parse for Expr {
    fn parse(stream: &mut TokenStream) -> Result<Self, MGLError> {
        Self::assignment(stream)
    }
}

impl Expr {
    fn assignment(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let expr = Self::or(stream)?;

        if stream.match_tokens(&[TokenType::Equal]) {
            let equals = stream.previous();
            let value = Self::assignment(stream)?;

            return match &expr {
                Self::Var(sym) => Ok(Self::Assign {
                    sym: sym.clone(),
                    value: Box::new(value),
                }),
                Self::Get { attr, lhs } => Ok(Self::Set {
                    lhs: lhs.clone(),
                    attr: attr.clone(),
                    rhs: Box::new(value),
                }),

                _ => {
                    let token = stream.peek();
                    Err(MGLError {
                        error_type: ErrorType::SyntaxError,
                        msg: format!("invalid assignment target '{}'", token.lexeme),
                        loc: Some(token.into()),
                    })
                }
            };
        }

        Ok(expr)
    }

    fn or(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let mut expr = Self::and(stream)?;

        while stream.match_tokens(&[TokenType::Or]) {
            let operator = stream.previous();
            let right = Self::and(stream)?;
            expr = Self::Binary {
                left: Box::new(expr),
                operator: operator.into(),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn and(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let mut expr = Self::equality(stream)?;
        while stream.match_tokens(&[TokenType::And]) {
            let operator = stream.previous();
            let right = Self::equality(stream)?;
            expr = Self::Binary {
                left: Box::new(expr),
                operator: operator.into(),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn equality(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let mut expr = Self::comparison(stream)?;

        while stream.match_tokens(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = stream.previous();
            let right = Self::comparison(stream)?;
            expr = Self::Binary {
                right: Box::new(right),
                operator: operator.into(),
                left: Box::new(expr),
            };
        }

        Ok(expr)
    }

    fn comparison(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let mut expr = Self::term(stream)?;

        while stream.match_tokens(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = stream.previous();
            let right = Self::term(stream)?;
            expr = Self::Binary {
                right: Box::new(right),
                operator: operator.into(),
                left: Box::new(expr),
            };
        }

        Ok(expr)
    }

    fn term(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let mut expr = Self::factor(stream)?;
        while stream.match_tokens(&[TokenType::Minus, TokenType::Plus]) {
            let operator = stream.previous();
            let right = Self::factor(stream)?;
            expr = Self::Binary {
                right: Box::new(right),
                operator: operator.into(),
                left: Box::new(expr),
            };
        }

        Ok(expr)
    }

    fn factor(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let mut expr = Self::unary(stream)?;

        while stream.match_tokens(&[TokenType::Slash, TokenType::Star]) {
            let operator = stream.previous();
            let right = Self::unary(stream)?;
            expr = Self::Binary {
                right: Box::new(right),
                operator: operator.into(),
                left: Box::new(expr),
            };
        }

        Ok(expr)
    }

    fn unary(stream: &mut TokenStream) -> Result<Self, MGLError> {
        if stream.match_tokens(&[TokenType::Bang, TokenType::Minus]) {
            let operator = stream.previous().token_type;
            let expr = Self::unary(stream)?;
            return Ok(Self::Unary {
                expr: Box::new(expr),
                operator: operator.into(),
            });
        }

        Self::primary(stream)
    }

    fn call(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let expr = Box::new(Self::primary(stream)?);

        if stream.match_tokens(&[TokenType::Dot]) {
            let attr = stream.consume(TokenType::Ident, String::from("expected attribute after '.'"))?.into();
            return Ok(Expr::Get { attr, lhs: expr });
        }

        let arg = Box::new(Self::primary(stream)?);

        let token = stream.peek();
        Ok(Expr::Call { loc: token.into(), arg, callee: expr })
    }

    /*fn array(&mut self, loc: SourceLocation) -> Result<Vec<Self>, MGLError> {
        let mut values = Vec::new();

        if !Self::check(&TokenType::RightBracket) {
            loop {
                values.push(Self::expression()?);

                if !Self::match_tokens(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        Self::consume_unclosed(TokenType::RightBracket, ']', loc)?
            .to_source_location();

        Ok(values)
    }*/

    fn primary(stream: &mut TokenStream) -> Result<Self, MGLError> {
        if stream.match_tokens(&[TokenType::While]) {
            return Ok(Self::While(WhileExpr::parse(stream)?));
        }

        if stream.match_tokens(&[TokenType::If]) {
            return Ok(Self::If(IfExpr::parse(stream)?));
        }

        if stream.match_tokens(&[TokenType::Return]) {
            return Ok(Self::Return { loc: stream.peek().into(), value: Box::new(Expr::parse(stream)?) });
        }

        if stream.match_tokens(&[TokenType::False]) {
            let token = stream.peek();
            return Ok(Self::Literal {
                lit: Literal::Bool(false),
                loc: token.into()
            });
        }
        if stream.match_tokens(&[TokenType::True]) {
            let token = stream.peek();
            return Ok(Self::Literal {
                lit: Literal::Bool(true),
                loc: token.into()
            });
        }

        if stream.match_tokens(&[TokenType::Number, TokenType::String]) {
            let (token, prev_token) = (stream.peek(), stream.previous());
            match prev_token.literal {
                Some(ref lit) => {
                    return Ok(Self::Literal {
                        lit: lit.clone(),
                        loc: prev_token.into()
                    });
                }
                None => {
                    return Err(MGLError {
                        error_type: ErrorType::SyntaxError,
                        msg: String::from("expected string or number"),
                        loc: Some(token.into()),
                    });
                }
            }
        }

        if stream.match_tokens(&[TokenType::SelfTy]) {
            return Ok(Self::This(stream.previous().into()));
        }

        if stream.match_tokens(&[TokenType::Ident]) {
            return Ok(Self::Var(stream.previous().into()));
        }

        if stream.match_tokens(&[TokenType::LeftParen]) {
            return Ok(Self::Group(Box::new(Self::parse(stream)?)));
        }

        /*if Self::match_tokens(&[TokenType::LeftBracket]) {
            return Ok(Self::List {
                loc: loc.clone(),
                exprs: Self::array(loc)?,
            });
        }*/

        Err(MGLError {
            error_type: ErrorType::SyntaxError,
            msg: String::from("Expect expression"),
            loc: Some(stream.peek().into()),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureDecl {
    pub param: (Symbol, Type),
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

impl Parse for IfExpr {
    fn parse(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let condition = Box::new(Expr::parse(stream)?);

        let then_branch = Box::new(Stmt::parse(stream)?);
        let else_branch = if stream.match_tokens(&[TokenType::Else]) {
            Some(Box::new(Stmt::parse(stream)?))
        } else {
            None
        };

        Ok(IfExpr {
            condition,
            then_branch,
            else_branch,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpr {
    condition: Box<Expr>,
    body: Box<Stmt>,
}

impl Parse for WhileExpr {
    fn parse(stream: &mut TokenStream) -> Result<Self, MGLError> {
        let condition = Box::new(Expr::parse(stream)?);

        let body = Box::new(Stmt::parse(stream)?);

        Ok(WhileExpr { condition, body })
    }
}
