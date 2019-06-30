//! Syntax Tree Library used throughout Mun.
//!
//! This crate is heavily inspired by Rust Analyzers ra_syntax crate.
//!
//! Properties:
//!     - easy and fast incremental re-parsing
//!     - graceful handling of errors
//!     - full-fidelity representation (*any* text can be precisely represented as
//!       a syntax tree)

pub mod ast;
mod parsing;
mod ptr;
mod syntax_error;
#[macro_use]
mod syntax_kind;
mod syntax_node;
mod syntax_text;

pub use crate::{
    ast::AstNode,
    parsing::{tokenize, Token},
    ptr::SyntaxNodePtr,
    syntax_error::{SyntaxError, SyntaxErrorKind},
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
        self.syntax.root_data().to_vec()
    }
}

/// This test does not assert anything and instead just shows off the crate's API.
#[test]
fn api_walkthrough() {
    use ast::ModuleItemOwner;
    use ast::{NameOwner};

    let source_code = "
        function foo() {

        }
    ";

    // `SourceFile` is the main entry point.
    //
    // Since all source can be parsed even invalid source code (which will result in a lot of
    // errors) the `parse` method does not return a `Result`.
    let file = SourceFile::parse(source_code);

    // `SourceFile` is the root of the syntax tree. We can iterate file's items:
    let mut func = None;
    for item in file.items() {
        match item.kind() {
            ast::ModuleItemKind::FunctionDef(f) => func = Some(f),
            _ => unreachable!(),
        }
    }

    // The returned items are always references.
    let func: &ast::FunctionDef = func.unwrap();

    // All nodes implement the `ToOwned` trait, which `Owned = TreeArc<Self>`. A `TreeArc` is
    // similar to `Arc` but references the root of the tree.
    let _owned_func: TreeArc<ast::FunctionDef> = func.to_owned();

    assert_eq!(func.name().unwrap().syntax().text(), "foo")
}
