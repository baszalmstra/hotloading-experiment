//! Syntax Tree Library used throughout Mun.
//!
//! This crate is heavily inspired by Rust Analyzers ra_syntax crate.
//!
//! Properties:
//!     - easy and fast incremental re-parsing
//!     - graceful handling of errors
//!     - full-fidelity representation (*any* text can be precisely represented as
//!       a syntax tree)
//!
//!

mod syntax_kind;
mod parsing;

pub use rowan::{SmolStr, TextRange, TextUnit};
pub use syntax_kind::SyntaxKind;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
