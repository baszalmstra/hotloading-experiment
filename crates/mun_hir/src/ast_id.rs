use crate::{db::DefDatabase, FileId, RawId, Arena};
use mun_syntax::{ast, AstNode, SyntaxNode, SyntaxNodePtr, TreeArc};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::Arc;

/// Maps items' `SyntaxNode`s to `ErasedFileAstId`s and back.
#[derive(Debug, PartialEq, Eq, Default)]
pub struct AstIdMap {
    arena: Arena<ErasedFileAstId, SyntaxNodePtr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErasedFileAstId(RawId);
impl_arena_id!(ErasedFileAstId);

impl AstIdMap {
    fn alloc(&mut self, item: &SyntaxNode) -> ErasedFileAstId {
        self.arena.alloc(SyntaxNodePtr::new(item))
    }

    pub(crate) fn ast_id_map_query(db: &impl DefDatabase, file_id: FileId) -> Arc<AstIdMap> {
        let map = AstIdMap::from_source(db.parse(file_id).syntax());
        Arc::new(map)
    }

    pub(crate) fn file_item_query(
        db: &impl DefDatabase,
        file_id: FileId,
        ast_id: ErasedFileAstId,
    ) -> TreeArc<SyntaxNode> {
        let node = db.parse(file_id);
        db.ast_id_map(file_id).arena[ast_id]
            .to_node(node.syntax())
            .to_owned()
    }

    pub(crate) fn ast_id<N: AstNode>(&self, item: &N) -> FileAstId<N> {
        let ptr = SyntaxNodePtr::new(item.syntax());
        let raw = match self.arena.iter().find(|(_id, i)| **i == ptr) {
            Some((it, _)) => it,
            None => panic!(
                "Can't find {:?} in AstIdMap:\n{:?}",
                item.syntax(),
                self.arena.iter().map(|(_id, i)| i).collect::<Vec<_>>(),
            ),
        };

        FileAstId { raw, _ty: PhantomData }
    }

    fn from_source(node: &SyntaxNode) -> AstIdMap {
        assert!(node.parent().is_none());
        let mut res = AstIdMap::default();
        // By walking the tree in bread-first order we make sure that parents
        // get lower ids then children. That is, adding a new child does not
        // change parent's id. This means that, say, adding a new function to a
        // trait does not change ids of top-level items, which helps caching.
        bfs(node, |it| {
            if let Some(module_item) = ast::ModuleItem::cast(it) {
                res.alloc(module_item.syntax());
            }
        });
        res
    }
}

/// Walks the subtree in bfs order, calling `f` for each node.
fn bfs(node: &SyntaxNode, mut f: impl FnMut(&SyntaxNode)) {
    let mut curr_layer = vec![node];
    let mut next_layer = vec![];
    while !curr_layer.is_empty() {
        curr_layer.drain(..).for_each(|node| {
            next_layer.extend(node.children());
            f(node);
        });
        std::mem::swap(&mut curr_layer, &mut next_layer);
    }
}

/// `AstId` points to an AST node in a specific file.
#[derive(Debug)]
pub(crate) struct FileAstId<N: AstNode> {
    raw: ErasedFileAstId,
    _ty: PhantomData<N>,
}

impl<N: AstNode> Clone for FileAstId<N> {
    fn clone(&self) -> FileAstId<N> {
        *self
    }
}
impl<N: AstNode> Copy for FileAstId<N> {}

impl<N: AstNode> PartialEq for FileAstId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}
impl<N: AstNode> Eq for FileAstId<N> {}
impl<N: AstNode> Hash for FileAstId<N> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.raw.hash(hasher);
    }
}

impl<N: AstNode> FileAstId<N> {
    pub(crate) fn with_file_id(self, file_id: FileId) -> AstId<N> {
        AstId {
            file_id,
            file_ast_id: self,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct AstId<N: AstNode> {
    file_id: FileId,
    file_ast_id: FileAstId<N>,
}

impl<N: AstNode> AstId<N> {
    pub(crate) fn file_id(&self) -> FileId {
        self.file_id
    }

    pub(crate) fn to_node(&self, db: &impl DefDatabase) -> TreeArc<N> {
        let syntax_node = db.ast_id_to_node(self.file_id, self.file_ast_id.raw);
        N::cast(&syntax_node).unwrap().to_owned()
    }
}
