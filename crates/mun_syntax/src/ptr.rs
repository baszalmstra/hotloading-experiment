use crate::{SyntaxKind, SyntaxNode, TextRange};
use std::iter::successors;

/// A pointer to a syntax node inside a file. It can be used to remember a
/// specific node across reparses of the same file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntaxNodePtr {
    pub(crate) range: TextRange,
    kind: SyntaxKind,
}

impl SyntaxNodePtr {
    pub fn new(node: &SyntaxNode) -> SyntaxNodePtr {
        SyntaxNodePtr {
            range: node.range(),
            kind: node.kind(),
        }
    }

    pub fn to_node(self, root: &SyntaxNode) -> &SyntaxNode {
        assert!(root.parent().is_none());
        successors(Some(root), |&node| {
            node.children()
                .find(|it| self.range.is_subrange(&it.range()))
        })
        .find(|it| it.range() == self.range && it.kind() == self.kind)
        .unwrap_or_else(|| panic!("can't resolve local ptr to SyntaxNode: {:?}", self))
    }

    pub fn range(self) -> TextRange {
        self.range
    }

    pub fn kind(self) -> SyntaxKind {
        self.kind
    }
}
