use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    sync::Arc,
};

use crate::{
    ast::{AtomTy, Ident, Stmt, Ty},
    program::Scope,
};

#[derive(Clone, PartialEq)]
pub enum Expr {
    Var(Ident),
    Lit(LitExpr),
    Lambda(Lambda),
    Call(Box<Expr>, Box<Expr>),
    Block(Block),
    Case(Box<Expr>, Vec<CaseArm>),
    Tuple(Vec<Expr>),
    Builtin(Builtin),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Var(var) => write!(f, "{var}"),
            Expr::Lit(lit) => write!(f, "{lit}"),
            Expr::Lambda(lambda) => write!(f, "{lambda}"),
            Expr::Call(expr, arg) => write!(f, "{expr} {arg}"),
            Expr::Block(block) => write!(f, "{block}"),
            Expr::Case(expr, vec) => {
                write!(f, "case")?;
                write!(f, " ")?;
                write!(f, "{expr}")?;
                for arm in vec {
                    write!(f, "{arm}")?;
                }
                Ok(())
            }
            Expr::Tuple(exprs) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    write!(f, "{expr}")?;
                    if i != exprs.len() - 1 {
                        f.write_str(",")?;
                    }
                }
                write!(f, ")")
            }
            Expr::Builtin(builtin) => write!(f, "{builtin:?}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitExpr {
    Bool(bool),
    Int(i128),
    Str(String),
}

impl LitExpr {
    pub fn ty(&self) -> Ty {
        match self {
            LitExpr::Bool(_) => Ty::Atom(AtomTy::Bool),
            LitExpr::Int(_) => Ty::Atom(AtomTy::Int),
            LitExpr::Str(_) => Ty::Atom(AtomTy::Str),
        }
    }
}

impl Display for LitExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LitExpr::Bool(bool) => write!(f, "{bool}"),
            LitExpr::Int(int) => write!(f, "{int}"),
            LitExpr::Str(_str) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseArm {
    pub pat: Pat,
    pub expr: Box<Expr>,
}

impl Display for CaseArm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}={}", self.pat, self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pat {
    Var(Ident),
    Num(i128),
    Wildcard,
}

impl Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pat::Var(ident) => write!(f, "{ident}"),
            Pat::Num(num) => write!(f, "{num}"),
            Pat::Wildcard => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub arg: LambdaArg,
    pub ret: Ty,
    pub expr: Box<Expr>,
}

impl Lambda {
    pub fn sig(&self) -> Ty {
        Ty::Fn(Box::new(self.arg.ty.clone()), Box::new(self.ret.clone()))
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // match &*self.arg {
        //     [] => write!(f, "()")?,
        //     [arg] => write!(f, "{arg} ")?,
        //     args => {
        //         for (i, arg) in args.iter().enumerate() {
        //             write!(f, "{arg}")?;
        //             if i != args.len() - 1 {
        //                 f.write_str(" ")?;
        //             }
        //         }
        //     }
        // }

        write!(f, "{}", self.arg)?;
        write!(f, "=>")?;
        write!(f, "{}", self.ret)?;
        write!(f, " ")?;
        write!(f, "{}", self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaArg {
    pub name: Ident,
    pub ty: Ty,
}

impl Display for LambdaArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        write!(f, ":")?;
        write!(f, "{}", self.ty)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub tail: Box<Expr>,
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for stmt in &self.stmts {
            write!(f, "{stmt}")?;
        }
        write!(f, "{}", self.tail)?;
        write!(f, "}}")
    }
}

#[derive(Clone)]
pub struct Builtin(Cow<'static, str>, Arc<dyn BuiltinFn>);

impl Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin {}>", self.0)
    }
}

impl PartialEq for Builtin {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Builtin {
    pub fn new(name: impl Into<Cow<'static, str>>, f: impl BuiltinFn) -> Self {
        Self(name.into(), Arc::new(f))
    }

    pub fn call(&self, scope: &Scope) -> anyhow::Result<Expr> {
        (self.1)(scope)
    }
}

pub trait BuiltinFn: Fn(&Scope<'_>) -> anyhow::Result<Expr> + 'static {}
impl<T> BuiltinFn for T where T: (Fn(&Scope<'_>) -> anyhow::Result<Expr>) + 'static {}
