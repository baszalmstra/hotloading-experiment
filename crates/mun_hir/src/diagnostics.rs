use std::{
    fmt,
    any::Any
};
use crate::{FileId, HirDatabase};
use mun_syntax::{SyntaxNodePtr, TextRange, SyntaxNode, AstNode};

/// Diagnostic defines hir API for errors and warnings.
///
/// It is used as a `dyn` object, which you can downcast to a concrete diagnostic. DiagnosticSink
/// are structured, meaning that they include rich information which can be used by IDE to create
/// fixes.
///
/// Internally, various subsystems of HIR produce diagnostic specific to a subsystem (typically,
/// an `enum`), which are safe to store in salsa but do not include source locations. Such internal
/// diagnostics are transformed into an instance of `Diagnostic` on demand.
pub trait Diagnostic: Any + Send + Sync + fmt::Debug + 'static {
    fn message(&self) -> String;
    fn file(&self) -> FileId;
    fn syntax_node_ptr(&self) -> SyntaxNodePtr;
    fn highlight_range(&self) -> TextRange {
        self.syntax_node_ptr().range()
    }
    fn as_any(&self) -> &(dyn Any + Send + 'static);
}

pub trait AstDiagnostic {
    type AST;
    fn ast(&self, db: &impl HirDatabase) -> Self::AST;
}

impl dyn Diagnostic {
    pub fn syntax_node(&self, db: &impl HirDatabase) -> SyntaxNode {
        let node = db.parse(self.file()).syntax_node();
        self.syntax_node_ptr().to_node(&node)
    }

    pub fn downcast_ref<D: Diagnostic>(&self) -> Option<&D> {
        self.as_any().downcast_ref()
    }
}