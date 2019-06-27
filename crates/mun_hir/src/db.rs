use crate::{ast_id::{AstIdMap, ErasedFileAstId}, line_index::LineIndex, FileId, RawItems, ids};
use mun_syntax::{SourceFile, SyntaxNode, TreeArc, ast};
use std::sync::Arc;
use crate::ast_id::AstId;
use crate::code_model::{FnData, Function};

/// Database which stores all significant input facts: source code and project model. Everything
/// else in rust-analyzer is derived from these queries.
#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: std::fmt::Debug {
    /// Text of the file.
    #[salsa::input]
    fn file_text(&self, file_id: FileId) -> Arc<String>;

    /// Parses the file into the syntax tree.
    #[salsa::invoke(parse_query)]
    fn parse(&self, file_id: FileId) -> TreeArc<SourceFile>;

    /// Returns the line index of a file
    #[salsa::invoke(line_index_query)]
    fn line_index(&self, file_id: FileId) -> Arc<LineIndex>;
}

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase {
    #[salsa::invoke(crate::ast_id::AstIdMap::ast_id_map_query)]
    fn ast_id_map(&self, file_id: FileId) -> Arc<AstIdMap>;

    #[salsa::invoke(crate::ast_id::AstIdMap::file_item_query)]
    fn ast_id_to_node(&self, file_id: FileId, ast_id: ErasedFileAstId) -> TreeArc<SyntaxNode>;

    #[salsa::invoke(RawItems::raw_file_items_query)]
    fn raw_items(&self, file_id: FileId) -> Arc<RawItems>;

    #[salsa::interned]
    fn intern_function(&self, loc: ids::ItemLoc<ast::FunctionDef>) -> ids::FunctionId;
}

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: DefDatabase {
    #[salsa::invoke(crate::FnData::fn_data_query)]
    fn fn_data(&self, func: Function) -> Arc<FnData>;
}

fn parse_query(db: &impl SourceDatabase, file_id: FileId) -> TreeArc<SourceFile> {
    let text = db.file_text(file_id);
    SourceFile::parse(&*text)
}

fn line_index_query(db: &impl SourceDatabase, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_text(file_id);
    Arc::new(LineIndex::new(text.as_ref()))
}
