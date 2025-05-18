use std::collections::HashMap;

use crate::{
    interpreter::Interpreter,
    saving::{
        error::*,
        symbol::{SourceLocation, Symbol},
        value::Identifier,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub struct Enviroment {
    pub enclosing: Option<Box<Enviroment>>,
    pub values: HashMap<String, (Identifier, SourceLocation)>,
}

impl Enviroment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn from(enclosing: Enviroment) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(Box::new(enclosing)),
        }
    }

    pub fn define(&mut self, sym: Symbol, ident: Identifier) {
        self.values.insert(
            sym.name,
            (
                ident,
                SourceLocation {
                    line: sym.line,
                    col: sym.col,
                },
            ),
        );
    }

    pub fn get(&self, sym: Symbol, interpreter: &Interpreter) -> Result<Identifier, Error> {
        match self.values.get(&sym.name) {
            Some((val, _)) => Ok(val.clone()),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.get(sym, interpreter),
                None => Err(interpreter.error_builder.build(
                    ErrorType::Runtime,
                    format!("undefined variable '{}'", sym.name),
                    SourceLocation {
                        line: sym.line,
                        col: sym.col,
                    },
                )),
            },
        }
    }

    pub fn assign(
        &mut self,
        sym: Symbol,
        ident: Identifier,
        interpreter: &Interpreter,
    ) -> Result<(), Error> {
        if let Some(saved) = self.values.get(&sym.name) {
            if ident.r#type != saved.0.r#type {
                return Err(interpreter.error_builder.build(
                    ErrorType::Type,
                    format!("expected value of type '{}'. Found '{}'", saved.0.r#type, ident.r#type),
                    saved.1.clone(),
                ))
            }
            self.define(sym, ident);
            return Ok(());
        }
        match &mut self.enclosing {
            Some(enclosing) => {
                enclosing.assign(sym, ident, interpreter)?;
                return Ok(());
            }
            None => Err(interpreter.error_builder.build(
                ErrorType::Runtime,
                format!("undefined variable '{}'", sym.name),
                SourceLocation {
                    line: sym.line,
                    col: sym.col,
                },
            )),
        }
    }
}
