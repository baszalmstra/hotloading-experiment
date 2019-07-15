///! HIR for references to types. These paths are not yet resolved. They can be directly created
/// from an `ast::TypeRef`, without further queries.
use crate::Path;
use mun_syntax::ast::{self, TypeRefKind};

/// Compare ty::Ty
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeRef {
    Path(Path),
    Error,
}

impl TypeRef {
    /// Converts an `ast::TypeRef` to a `hir:TypeRef`.
    pub(crate) fn from_ast(node: &ast::TypeRef) -> Self {
        use mun_syntax::ast::TypeRefKind::*;
        match node.kind() {
            PathType(path) => path
                .path()
                .and_then(Path::from_ast)
                .map(TypeRef::Path)
                .unwrap_or(TypeRef::Error),
        }
    }

    pub(crate) fn from_ast_opt(node: Option<&ast::TypeRef>) -> Self {
        if let Some(node) = node {
            TypeRef::from_ast(node)
        } else {
            TypeRef::Error
        }
    }
}
