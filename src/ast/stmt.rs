use std::fmt::Display;

use crate::ast::{Expr, Ident};

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub ident: Ident,
    pub value: Expr,
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)?;
        write!(f, "=")?;
        write!(f, "{}", self.value)?;
        write!(f, ";")
    }
}
