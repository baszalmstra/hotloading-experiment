//! HIR provides a high-level object oriented access to Mun code.

pub mod line_index;
mod db;
mod ast_id;

pub use ::salsa as salsa;
use std::sync::Arc;
use mun_syntax::{TreeArc, SourceFile};
use crate::line_index::LineIndex;

pub use crate::{
    db::{SourceDatabase, SourceDatabaseStorage, DefDatabase, DefDatabaseStorage}
};

/// `FileId` is an integer which uniquely identifies a file. File paths are messy and
/// system-dependent, so most of the code should work directly with `FileId`, without inspecting the
/// path. The mapping between `FileId` and path and `SourceRoot` is constant. A file rename is
/// represented as a pair of deletion/creation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub u32);



