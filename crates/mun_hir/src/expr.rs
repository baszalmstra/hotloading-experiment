use crate::{arena::{RawId, Arena}, code_model::DefWithBody, Path, arena::map::ArenaMap, HirDatabase, FileId};

//pub use mun_syntax::ast::PrefixOp as UnaryOp;
pub use mun_syntax::ast::BinOp as BinaryOp;
use rustc_hash::FxHashMap;
use mun_syntax::{SyntaxNodePtr};
use std::sync::Arc;
use crate::code_model::src::HasSource;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId(RawId);
impl_arena_id!(ExprId);

/// The body of an item (function, const etc.).
#[derive(Debug, Eq, PartialEq)]
pub struct Body {
    owner: DefWithBody,
    exprs: Arena<ExprId, Expr>,
}

/// An item body together with the mapping from syntax nodes to HIR expression Ids. This is needed
/// to go from e.g. a position in a file to the HIR expression containing it; but for type
/// inference etc., we want to operate on a structure that is agnostic to the action positions of
/// expressions in the file, so that we don't recompute types whenever some whitespace is typed.
#[derive(Default, Debug, Eq, PartialEq)]
pub struct BodySourceMap {
    expr_map: FxHashMap<SyntaxNodePtr, ExprId>,
    expr_map_back: ArenaMap<ExprId, SyntaxNodePtr>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    /// Used if the syntax tree does not have a required expression piece
    Missing,
    Path(Path),
    BinaryOp {
        lhs: ExprId,
        rhs: ExprId,
        op: Option<BinaryOp>,
    },
}

// Queries

pub(crate) struct ExprCollector<DB> {
    db: DB,
    owner: DefWithBody,
    exprs: Arena<ExprId, Expr>,
    source_map: BodySourceMap,
}

impl<'a, DB> ExprCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn new(owner: DefWithBody, file_id: FileId, db: &'a DB) -> Self {
        ExprCollector {
            owner,
            db,
            exprs: Arena::default(),
            source_map: BodySourceMap::default(),
        }
    }

    fn finish(self) -> (Body, BodySourceMap) {
        let body = Body {
            owner: self.owner,
            exprs: self.exprs,
//            pats: self.pats,
//            params: self.params,
//            body_expr: self.body_expr.expect("A body should have been collected"),
        };
        (body, self.source_map)
    }
}

pub(crate) fn body_with_source_map_query(
    db: &impl HirDatabase,
    def: DefWithBody,
) -> (Arc<Body>, Arc<BodySourceMap>) {
    let mut collector;

    match def {
        DefWithBody::Function(ref f) => {
            let src = f.source(db);
            collector = ExprCollector::new(def, src.file_id, db);
            //collector.collect_fn_body(&src.ast)
        }
    }

    let (body, source_map) = collector.finish();
    (Arc::new(body), Arc::new(source_map))
}

pub(crate) fn body_hir_query(db: &impl HirDatabase, def: DefWithBody) -> Arc<Body> {
    db.body_with_source_map(def).0
}
