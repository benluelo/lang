use std::{borrow::Cow, fmt::Display};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(Cow<'static, str>);

impl std::fmt::Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Ident {
    pub fn new(s: &str) -> Option<Self> {
        is_valid_identifier(s).then_some(Self(Cow::Owned(s.to_owned())))
    }

    pub const fn new_static(s: &'static str) -> Self {
        if is_valid_identifier(s) {
            Self(Cow::Borrowed(s))
        } else {
            panic!("invalid identifier")
        }
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

const fn is_valid_identifier(s: &str) -> bool {
    !s.is_empty()
        && s.as_bytes()[0].is_ascii_alphabetic()
        && 'block: {
            let mut i = 0;
            while i < s.len() {
                if !s.as_bytes()[i].is_ascii_alphanumeric() {
                    break 'block false;
                }
                i += 1;
            }
            true
        }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[macro_export]
macro_rules! ident {
    ($lit:literal) => {
        const { $crate::ast::ident::Ident::new_static($lit) }
    };
}
