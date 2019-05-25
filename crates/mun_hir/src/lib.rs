//! HIR provides a high-level object oriented access to Mun code.

pub mod line_index;

pub use ::salsa as salsa;
use std::sync::Arc;
use mun_syntax::{TreeArc, SourceFile};
use crate::line_index::LineIndex;

/// `FileId` is an integer which uniquely identifies a file. File paths are messy and
/// system-dependent, so most of the code should work directly with `FileId`, without inspecting the
/// path. The mapping between `FileId` and path and `SourceRoot` is constant. A file rename is
/// represented as a pair of deletion/creation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub u32);

/// Database which stores all significant input facts: source code and project model. Everything
/// else in rust-analyzer is derived from these queries.
#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: std::fmt::Debug {
    /// Text of the file.
    #[salsa::input]
    fn file_text(&self, file_id: FileId) -> Arc<String>;

    // Parses the file into the syntax tree.
    #[salsa::invoke(parse_query)]
    fn parse(&self, file_id: FileId) -> TreeArc<SourceFile>;

    // Returns the line index of a file
    #[salsa::invoke(line_index_query)]
    fn line_index(&self, file_id: FileId) -> Arc<LineIndex>;
}

fn parse_query(db: &impl SourceDatabase, file_id: FileId) -> TreeArc<SourceFile> {
    let text = db.file_text(file_id);
    SourceFile::parse(&*text)
}

fn line_index_query(db: &impl SourceDatabase, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_text(file_id);
    Arc::new(LineIndex::new(text.as_ref()))
}