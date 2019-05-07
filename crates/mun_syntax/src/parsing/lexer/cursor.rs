use crate::TextUnit;

use std::str::Chars;

/// A simple view into the characters of a string.
pub(crate) struct Cursor<'s> {
    text: &'s str,
    len: TextUnit
}

impl<'s> Cursor<'s> {
    /// Creates a new `Cursor` from a string.
    pub fn new(text: &'s str) -> Cursor<'s> {
        Cursor {text, len: 0.into()}
    }

    /// Gets the length of the remaining string.
    pub fn into_len(self) -> TextUnit {
        self.len
    }

    /// Gets the current character, if one exists
    pub fn current(&self) -> Option<char> {
        self.chars().next()
    }

    /// Gets the nth character from the current offset. For example, 0 will return the current
    /// character, 1 will return the next, etc.
    pub fn nth(&self, n: u32) -> Option<char> {
        self.chars().nth(n as usize)
    }

    /// Checks whether the current character is the specified character.
    pub fn matches(&self, c: char) -> bool {
        self.current() == Some(c)
    }

    /// Checks whether the current characters match the specified string.
    pub fn matches_str(&self, s: &str) -> bool {
        let chars = self.chars();
        chars.as_str().starts_with(s)
    }

    /// Checks whether the current character satisfies the specified predicate
    pub fn matches_if<F: Fn(char) -> bool>(&self, predicate: F) -> bool {
        self.current().map(predicate) == Some(true)
    }

    /// Checks whether the nth character satisfies the specified predicate
    pub fn matches_nth_if<F: Fn(char) -> bool>(&self, n: u32, predicate: F) -> bool {
        self.nth(n).map(predicate) == Some(true)
    }

    /// Move to the next character
    pub fn bump(&mut self) -> Option<char> {
        let ch = self.chars().next()?;
        self.len += TextUnit::of_char(ch);
        Some(ch)
    }

    /// Moves to the next character as long as `predicate` is satisfied.
    pub fn bump_while<F: Fn(char) -> bool>(&mut self, predicate: F) {
        loop {
            match self.current() {
                Some(c) if predicate(c) => {
                    self.bump();
                }
                _ => return,
            }
        }
    }

    /// Returns the text up to the current point.
    pub fn current_token_text(&self) -> &str {
        let len: u32 = self.len.into();
        &self.text[..len as usize]
    }

    /// Returns an iterator over the remaining characters.
    fn chars(&self) -> Chars {
        let len:u32 = self.len.into();
        self.text[len as usize..].chars()
    }
}