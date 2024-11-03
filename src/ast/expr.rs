use std::{
    borrow::Cow,
    collections::VecDeque,
    fmt::{self, Debug, Display},
    sync::Arc,
};

use crate::{
    ast::{AtomTy, FnTy, Ident, Stmt, Ty},
    program::type_check,
};

#[derive(Clone, PartialEq)]
pub enum Expr {
    Symbol(Ident),
    Lit(LitExpr),
    Lambda(Lambda),
    Call(Box<Expr>, Box<Expr>),
    Block(Block),
    Case(Box<Expr>, Vec<CaseArm>),
    Tuple(Vec<Expr>),

    Builtin(Builtin),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Symbol(symbol) => write!(f, "{symbol}"),
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
}

impl LitExpr {
    pub fn ty(&self) -> Ty {
        match self {
            LitExpr::Bool(_) => Ty::Atom(AtomTy::Bool),
            LitExpr::Int(_) => Ty::Atom(AtomTy::Int),
        }
    }
}

impl Display for LitExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LitExpr::Bool(bool) => write!(f, "{bool}"),
            LitExpr::Int(int) => write!(f, "{int}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseArm {
    pub pat: Pat,
    pub expr: Box<Expr>,
}

impl Display for CaseArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}={}", self.pat, self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pat {
    Symbol(Ident),
    Num(i128),
    Wildcard,
}

impl Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Symbol(ident) => write!(f, "{ident}"),
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
    pub fn sig(&self) -> FnTy {
        FnTy(self.arg.ty.clone(), self.ret.clone())
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        write!(f, ":")?;
        write!(f, "{}", self.ty)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: VecDeque<Stmt>,
    pub tail: Box<Expr>,
}

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for stmt in &self.stmts {
            write!(f, "{stmt}")?;
        }
        write!(f, "{}", self.tail)?;
        write!(f, "}}")
    }
}

#[derive(Clone)]
pub struct Builtin {
    name: Cow<'static, str>,
    ty: Ty,
    ret: Ty,
    f: Arc<dyn BuiltinFn>,
}

impl Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin {} ({})>", self.name, self.sig())
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty && Arc::ptr_eq(&self.f, &other.f)
    }
}

impl Builtin {
    pub fn new(name: impl Into<Cow<'static, str>>, ty: Ty, ret: Ty, f: impl BuiltinFn) -> Self {
        Self {
            name: name.into(),
            ty,
            ret,
            f: Arc::new(f),
        }
    }

    pub fn name(&self) -> &Cow<'static, str> {
        &self.name
    }

    pub fn call(&self, expr: Expr) -> anyhow::Result<Expr> {
        type_check(&self.ty, &expr)?;

        let ret = (self.f)(expr)?;

        type_check(&self.ret, &ret)?;

        Ok(ret)
    }

    pub fn sig(&self) -> FnTy {
        FnTy(self.ty.clone(), self.ret.clone())
    }
}

pub trait BuiltinFn: Fn(Expr) -> anyhow::Result<Expr> + 'static {}
impl<T> BuiltinFn for T where T: (Fn(Expr) -> anyhow::Result<Expr>) + 'static {}
