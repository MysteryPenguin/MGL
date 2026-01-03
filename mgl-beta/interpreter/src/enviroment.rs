use std::collections::HashMap;

use crate::{error::{ErrorType, MGLError}, loc::SourceLoc, saving::{
    callable::{ClassInst, Function}, literal::Literal, symbol::Symbol, r#type::Type
}};

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalEnv {
    pub env: Enviroment,
    pub class_instances: HashMap<usize, ClassInst>,
    pub functions: HashMap<usize, Function>
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Enviroment {
    pub enclosing: Option<Box<Enviroment>>,
    pub values: HashMap<Box<str>, (Type, Literal, SourceLoc)>,
}

impl Enviroment {
    pub fn from(enclosing: Enviroment) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(Box::new(enclosing)),
        }
    }

    pub fn define(&mut self, sym: Symbol, lit: Literal, r#type: Option<Type>) -> Result<(), MGLError> {
        self.values.insert(
            sym.name,
            (
                if let Some(r#type) = r#type {
                    if r#type != lit.to_type() {
                        return Err(MGLError { error_type: ErrorType::TypeError, msg: format!("value of type '{}' is not assignable to type '{}'", lit.to_type(), r#type), loc: Some((&sym).into()) });
                    }
                    r#type
                } else {
                    lit.to_type()
                },
                lit,
                SourceLoc {
                    line: sym.line,
                    col: sym.col
                }
            ),
        );
        Ok(())
    }

    pub fn get(&self, sym: &Symbol) -> Result<&Literal, MGLError> {
        match self.values.get(&sym.name) {
            Some((_, lit, _)) => Ok(lit),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.get(sym),
                None => Err(MGLError { error_type: ErrorType::UndefinedVariableError, msg: format!("variable '{}' does not exist", sym.name), loc: Some(sym.into()) }),
            },
        }
    }

    pub fn assign(
        &mut self,
        sym: Symbol,
        lit: Literal,
    ) -> Result<(), MGLError> {
        if let Some((r#type, lit, source_loc)) = self.values.get(&sym.name) {
            let lit_type = lit.to_type();
            self.define(sym, lit.clone(), Some(r#type.clone()));
            return Ok(());
        }
        match &mut self.enclosing {
            Some(enclosing) => {
                enclosing.assign(sym, lit)?;
                Ok(())
            }
            None => Err(MGLError { error_type: ErrorType::UndefinedVariableError, msg: format!("variable '{}' does not exist", sym.name), loc: Some((&sym).into()) })
        }
    }
}
