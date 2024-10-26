use std::fmt::Display;

use crate::ast::Ident;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Atom(AtomTy),
    /// (a, b, c, ...)
    Tuple(Vec<Ty>),
    /// a -> b
    Fn(Box<Ty>, Box<Ty>),
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Atom(atom) => write!(f, "{atom}"),
            Ty::Tuple(vec) => {
                write!(f, "(")?;
                for (i, ty) in vec.iter().enumerate() {
                    write!(f, "{ty}")?;
                    if i == vec.len() - 1 {
                        write!(f, ",")?;
                    }
                }
                write!(f, ")")
            }
            Ty::Fn(a, b) => {
                write!(f, "{a}->{b}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AtomTy {
    Int,
    Bool,
    Str,
    Named(Ident),
}

impl AtomTy {
    pub fn from_ident(i: Ident) -> Self {
        match i.as_str() {
            "int" => AtomTy::Int,
            "bool" => AtomTy::Bool,
            "str" => AtomTy::Str,
            _ => AtomTy::Named(i),
        }
    }
}

impl Display for AtomTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AtomTy::Int => f.write_str("int"),
            AtomTy::Bool => f.write_str("bool"),
            AtomTy::Str => f.write_str("str"),
            AtomTy::Named(ident) => write!(f, "{ident}"),
        }
    }
}
