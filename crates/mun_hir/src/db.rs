use crate::ast_id::AstId;
use crate::code_model::{FnData, Function, ModuleData, DefWithBody};
use crate::{
    ast_id::{AstIdMap, ErasedFileAstId},
    ids,
    line_index::LineIndex,
    FileId, PackageInput, RawItems,
};
use mun_syntax::{ast, SourceFile, SyntaxNode, TreeArc};
use std::sync::Arc;

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

    /// The input to the package
    #[salsa::input]
    fn package_input(&self) -> Arc<PackageInput>;
}

#[salsa::query_group(DefDatabaseStorage)]
pub trait DefDatabase: SourceDatabase {
    /// Returns the top level AST items of a file
    #[salsa::invoke(crate::ast_id::AstIdMap::ast_id_map_query)]
    fn ast_id_map(&self, file_id: FileId) -> Arc<AstIdMap>;

    /// Returns the corresponding AST node of a type erased ast id
    #[salsa::invoke(crate::ast_id::AstIdMap::file_item_query)]
    fn ast_id_to_node(&self, file_id: FileId, ast_id: ErasedFileAstId) -> TreeArc<SyntaxNode>;

    /// Returns the raw items of a file
    #[salsa::invoke(RawItems::raw_file_items_query)]
    fn raw_items(&self, file_id: FileId) -> Arc<RawItems>;

    /// Interns a function definition
    #[salsa::interned]
    fn intern_function(&self, loc: ids::ItemLoc<ast::FunctionDef>) -> ids::FunctionId;
}

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: DefDatabase {
    #[salsa::invoke(crate::FnData::fn_data_query)]
    fn fn_data(&self, func: Function) -> Arc<FnData>;

    /// Returns the module data of the specified file
    #[salsa::invoke(crate::code_model::ModuleData::module_data_query)]
    fn module_data(&self, file_id: FileId) -> Arc<ModuleData>;

    #[salsa::invoke(crate::expr::body_hir_query)]
    fn body_hir(&self, def: DefWithBody) -> Arc<crate::expr::Body>;

    #[salsa::invoke(crate::expr::body_with_source_map_query)]
    fn body_with_source_map(
        &self,
        def: DefWithBody,
    ) -> (Arc<crate::expr::Body>, Arc<crate::expr::BodySourceMap>);
}

fn parse_query(db: &impl SourceDatabase, file_id: FileId) -> TreeArc<SourceFile> {
    let text = db.file_text(file_id);
    SourceFile::parse(&*text)
}

fn line_index_query(db: &impl SourceDatabase, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_text(file_id);
    Arc::new(LineIndex::new(text.as_ref()))
}
