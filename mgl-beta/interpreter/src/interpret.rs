use crate::{enviroment::{Enviroment, GlobalEnv}, error::MGLError};

pub trait Interpret {
    fn interpret(&mut self, env: &mut GlobalEnv) -> Result<(), MGLError>;
}