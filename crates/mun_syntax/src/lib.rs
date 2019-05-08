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

mod parsing;
mod syntax_error;
mod syntax_kind;
mod syntax_node;
mod syntax_text;

pub use rowan::{SmolStr, TextRange, TextUnit};
pub use crate::{
    syntax_kind::SyntaxKind,
    syntax_error::{SyntaxError, SyntaxErrorKind, Location},
    syntax_text::SyntaxText,
    syntax_node::{Direction, SyntaxNode, WalkEvent, TreeArc, SyntaxTreeBuilder, SyntaxElement, SyntaxToken, InsertPosition},
    parsing::{Token}
};
