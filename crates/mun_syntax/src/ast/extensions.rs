use crate::SmolStr;
use crate::SyntaxKind;
use crate::{
    ast::{self, AstNode},
    T,
};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathSegmentKind<'a> {
    Name(&'a ast::NameRef),
    SelfKw,
    SuperKw,
}

impl ast::PathSegment {
    pub fn parent_path(&self) -> &ast::Path {
        self.syntax()
            .parent()
            .and_then(ast::Path::cast)
            .expect("segments are always nested in paths")
    }

    pub fn kind(&self) -> Option<PathSegmentKind> {
        let res = if let Some(name_ref) = self.name_ref() {
            PathSegmentKind::Name(name_ref)
        } else {
            match self.syntax().first_child_or_token()?.kind() {
                T![self] => PathSegmentKind::SelfKw,
                T![super] => PathSegmentKind::SuperKw,
                _ => return None,
            }
        };
        Some(res)
    }

    pub fn has_colon_colon(&self) -> bool {
        match self.syntax.first_child_or_token().map(|s| s.kind()) {
            Some(T![::]) => true,
            _ => false,
        }
    }
}
