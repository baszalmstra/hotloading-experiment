mod cursor;

use crate::{SyntaxKind, TextUnit};
use self::cursor::Cursor;

/// A token of Mun source
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    /// The kind of token
    pub kind: SyntaxKind,

    /// The length of the token
    pub len: TextUnit
}

/// Break a string up into its component tokens
pub fn tokenize(text: &str) -> Vec<Token> {
    let mut text = text;
    let mut result = Vec::new();
    while !text.is_empty() {
        let token = next_token(text);
        result.push(token);
        let len: u32 = token.len.into();
        text = &text[len as usize..];
    }
    result
}

/// Get the next token from a string
pub fn next_token(text: &str) -> Token {
    assert!(!text.is_empty());
    let mut ptr = Cursor::new(text);
    let c = ptr.bump().unwrap();
    let kind = next_token_inner(c, &mut ptr);
    let len = ptr.into_len();
    Token { kind, len }
}