use crate::{SyntaxKind, SyntaxNode, TextRange, AstNode};
use std::iter::successors;
use std::marker::PhantomData;

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

/// Like `SyntaxNodePtr`, but remembers the type of node
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AstPtr<N: AstNode> {
    raw: SyntaxNodePtr,
    _ty: PhantomData<N>,
}

impl<N: AstNode> Copy for AstPtr<N> {}
impl<N: AstNode> Clone for AstPtr<N> {
    fn clone(&self) -> AstPtr<N> {
        *self
    }
}

impl<N: AstNode> AstPtr<N> {
    pub fn new(node: &N) -> AstPtr<N> {
        AstPtr { raw: SyntaxNodePtr::new(node.syntax()), _ty: PhantomData }
    }

    pub fn to_node(self, root: &SyntaxNode) -> &N {
        let syntax_node = self.raw.to_node(root);
        N::cast(syntax_node).unwrap()
    }

    pub fn syntax_node_ptr(self) -> SyntaxNodePtr {
        self.raw
    }
}

impl<N: AstNode> From<AstPtr<N>> for SyntaxNodePtr {
    fn from(ptr: AstPtr<N>) -> SyntaxNodePtr {
        ptr.raw
    }
}