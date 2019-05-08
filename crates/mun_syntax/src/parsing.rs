use crate::SyntaxKind;

mod lexer;
mod text_token_source;

pub use lexer::{Token};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError(pub String);

/// `TokenSource` abstract the source of the tokens.
trait TokenSource {
    /// Returns the token at `pos`
    fn token_kind(&self, pos: usize) -> SyntaxKind;

    /// Returns true if the token at `pos` is joined to the next token. For example
    /// * `. .` -> not joined
    /// * `..` -> tokens are joined
    fn is_token_joint_to_next(&self, pos: usize) -> bool;
}