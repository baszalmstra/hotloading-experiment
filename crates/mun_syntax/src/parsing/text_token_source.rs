use crate::{
    parsing::{lexer::Token, TokenSource},
    SyntaxKind::{self, *},
    TextUnit,
};

pub(crate) struct TextTokenSource<'t> {
    text: &'t str,

    /// Holds the start position of each token
    start_offsets: Vec<TextUnit>,

    /// Non-whitespace/comment tokens
    tokens: Vec<Token>,
}

impl<'t> TokenSource for TextTokenSource<'t> {
    fn token_kind(&self, pos: usize) -> SyntaxKind {
        if pos >= self.tokens.len() {
            EOF
        } else {
            self.tokens[pos].kind
        }
    }

    fn is_token_joint_to_next(&self, pos: usize) -> bool {
        if (pos + 1) >= self.tokens.len() {
            true
        } else {
            self.start_offsets[pos] + self.tokens[pos].len == self.start_offsets[pos + 1]
        }
    }
}

impl<'t> TextTokenSource<'t> {
    /// Generate input for tokens (expect comment and whitespace).
    pub fn new(text: &'t str, raw_tokens: &'t [Token]) -> TextTokenSource<'t> {
        let mut tokens = Vec::new();
        let mut start_offsets = Vec::new();
        let mut len = 0.into();
        for &token in raw_tokens.iter() {
            if !token.kind.is_trivia() {
                tokens.push(token);
                start_offsets.push(len);
            }
            len += token.len;
        }

        TextTokenSource {
            text,
            start_offsets,
            tokens,
        }
    }
}
