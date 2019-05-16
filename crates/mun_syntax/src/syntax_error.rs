use crate::{parsing::ParseError, TextRange, TextUnit};
use std::cmp::min;
use std::fmt;
use mun_errors::{Location, Diagnostic};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxError {
    kind: SyntaxErrorKind,
    location: Location,
}

impl SyntaxError {
    pub fn new<L: Into<Location>>(kind: SyntaxErrorKind, loc: L) -> SyntaxError {
        SyntaxError {
            kind,
            location: loc.into(),
        }
    }

    pub fn kind(&self) -> SyntaxErrorKind {
        self.kind.clone()
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SyntaxErrorKind {
    ParseError(ParseError),
}

impl fmt::Display for SyntaxErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::SyntaxErrorKind::*;
        match self {
            ParseError(msg) => write!(f, "{}", msg.0),
        }
    }
}

impl Into<mun_errors::Diagnostic> for SyntaxError {
    fn into(self) -> mun_errors::Diagnostic {
        Diagnostic {
            level: mun_errors::Level::Error,
            loc: self.location,
            message: format!("{}", self.kind)
        }
    }
}