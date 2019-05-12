use crate::line_index::{LineCol, LineIndex};
use colored::*;
use mun_syntax::SyntaxError;
use std::fmt;

pub struct Diagnostic {
    line_col: LineCol,
    err: SyntaxError,
}

impl Diagnostic {
    pub fn new(line_index: &LineIndex, err: SyntaxError) -> Diagnostic {
        let line_col = line_index.line_col(err.offset());
        Diagnostic { line_col, err }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} ({}:{}): {}",
            "error".red(),
            self.line_col.line + 1,
            self.line_col.col,
            self.err
        )
    }
}
