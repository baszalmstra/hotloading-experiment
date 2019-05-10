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

mod ast;
mod parsing;
mod syntax_error;
mod syntax_kind;
mod syntax_node;
mod syntax_text;

pub use crate::{
    ast::AstNode,
    parsing::{tokenize, Token},
    syntax_error::{Location, SyntaxError, SyntaxErrorKind},
    syntax_kind::SyntaxKind,
    syntax_node::{
        Direction, InsertPosition, SyntaxElement, SyntaxNode, SyntaxToken, SyntaxTreeBuilder,
        TreeArc, WalkEvent,
    },
    syntax_text::SyntaxText,
};
pub use rowan::{SmolStr, TextRange, TextUnit};

/// `SourceFile` represents a parse tree for a single Mun file.
pub use crate::ast::SourceFile;
use rowan::GreenNode;

impl SourceFile {
    fn new(green: GreenNode, errors: Vec<SyntaxError>) -> TreeArc<SourceFile> {
        let root = SyntaxNode::new(green, errors);
        assert_eq!(root.kind(), SyntaxKind::SOURCE_FILE);
        TreeArc::cast(root)
    }

    pub fn parse(text: &str) -> TreeArc<SourceFile> {
        let (green, errors) = parsing::parse_text(text);
        SourceFile::new(green, errors)
    }

    pub fn errors(&self) -> Vec<SyntaxError> {
        let mut errors = self.syntax.root_data().to_vec();
        errors
    }
}
