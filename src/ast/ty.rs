use std::fmt::{self, Debug, Display};

use crate::ast::Ident;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Atom(AtomTy),
    /// (a, b, c, ...)
    Tuple(Vec<Ty>),
    Fn(Box<FnTy>),
}

impl Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Atom(atom) => write!(f, "{atom}"),
            Ty::Tuple(vec) => {
                write!(f, "(")?;
                for (i, ty) in vec.iter().enumerate() {
                    write!(f, "{ty}")?;
                    if i != vec.len() - 1 {
                        write!(f, ",")?;
                    }
                }
                write!(f, ")")
            }
            Ty::Fn(fn_ty) => {
                write!(f, "{fn_ty}")
            }
        }
    }
}

/// a -> b
#[derive(Clone, PartialEq, Eq)]
pub struct FnTy(pub Ty, pub Ty);

impl Debug for FnTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}->{}", self.0, self.1)
    }
}

impl Display for FnTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AtomTy {
    Int,
    Bool,
    Named(Ident),
}

impl AtomTy {
    pub fn from_ident(i: Ident) -> Self {
        match i.as_str() {
            "int" => AtomTy::Int,
            "bool" => AtomTy::Bool,
            _ => AtomTy::Named(i),
        }
    }

    pub const fn from_ident_static(i: &'static str) -> Self {
        // https://internals.rust-lang.org/t/why-i-cannot-compare-two-static-str-s-in-a-const-context/17726/8
        const fn const_bytes_equal(lhs: &[u8], rhs: &[u8]) -> bool {
            if lhs.len() != rhs.len() {
                return false;
            }
            let mut i = 0;
            while i < lhs.len() {
                if lhs[i] != rhs[i] {
                    return false;
                }
                i += 1;
            }
            true
        }

        if const_bytes_equal("int".as_bytes(), i.as_bytes()) {
            AtomTy::Int
        } else if const_bytes_equal("bool".as_bytes(), i.as_bytes()) {
            AtomTy::Bool
        } else {
            AtomTy::Named(crate::ast::ident::Ident::new_static(i))
        }
    }
}

impl Display for AtomTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AtomTy::Int => f.write_str("int"),
            AtomTy::Bool => f.write_str("bool"),
            AtomTy::Named(ident) => write!(f, "{ident}"),
        }
    }
}
