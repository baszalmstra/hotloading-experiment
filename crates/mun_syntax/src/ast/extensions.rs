use crate::ast::{self, AstNode};
use crate::SmolStr;

impl ast::Name {
    pub fn text(&self) -> &SmolStr {
        let ident = self
            .syntax()
            .first_child_or_token()
            .unwrap()
            .as_token()
            .unwrap();
        ident.text()
    }
}

impl ast::NameRef {
    pub fn text(&self) -> &SmolStr {
        let ident = self
            .syntax()
            .first_child_or_token()
            .unwrap()
            .as_token()
            .unwrap();
        ident.text()
    }
}
