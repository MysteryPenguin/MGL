use std::collections::HashMap;

use crate::saving::{
    error::*,
    literal::Identifier,
    symbol::{SourceLocation, Symbol},
};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Enviroment {
    pub enclosing: Option<Box<Enviroment>>,
    pub values: HashMap<String, (Identifier, SourceLocation)>,
}

impl Enviroment {
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

    pub fn get(&self, sym: &Symbol, error_builder: &ErrorBuilder) -> Result<Identifier, Error> {
        match self.values.get(&sym.name) {
            Some((val, _)) => Ok(val.clone()),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.get(sym, error_builder),
                None => Err(error_builder.build(ErrorType::Undefined {
                    ident: sym.name.clone(),
                    kind: String::from("variable"),
                    on: String::from("this module"),
                    loc: [SourceLocation {
                        line: sym.line,
                        col: sym.col,
                    }],
                })),
            },
        }
    }

    pub fn assign(
        &mut self,
        sym: Symbol,
        ident: Identifier,
        error_builder: &ErrorBuilder,
    ) -> Result<(), Error> {
        if let Some(saved) = self.values.get(&sym.name) {
            if ident.r#type != saved.0.r#type {
                return Err(error_builder.build(ErrorType::Type {
                    expected: saved.0.r#type.clone(),
                    found: ident.r#type,
                    loc: [sym.to_source_loc(), saved.1.clone()],
                }));
            }
            self.define(sym, ident);
            return Ok(());
        }
        match &mut self.enclosing {
            Some(enclosing) => {
                enclosing.assign(sym, ident, error_builder)?;
                Ok(())
            }
            None => Err(error_builder.build(ErrorType::Undefined {
                ident: sym.name.clone(),
                kind: String::from("variable"),
                on: String::from("this module"),
                loc: [SourceLocation {
                    line: sym.line,
                    col: sym.col,
                }],
            })),
        }
    }
}
