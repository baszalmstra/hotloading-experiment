//! HIR provides a high-level object oriented access to Mun code.

#[macro_use]
mod arena;
mod ast_id;
mod db;
pub mod line_index;
mod model;
mod name;
mod raw;
mod code_model;
mod ids;

use mun_syntax::{TreeArc};
pub use salsa;

pub use crate::{
    db::{HirDatabase, HirDatabaseStorage, DefDatabase, DefDatabaseStorage, SourceDatabase, SourceDatabaseStorage},
    name::{Name},
    raw::RawItems,
    ids::ItemLoc
};

use crate::{
    arena::{Arena, ArenaId, RawId},
    ast_id::{AstId, AstIdMap, FileAstId},
    line_index::LineIndex,
    name::{AsName},
};

pub use self::code_model::{
    Function, FnData
};

/// `FileId` is an integer which uniquely identifies a file. File paths are messy and
/// system-dependent, so most of the code should work directly with `FileId`, without inspecting the
/// path. The mapping between `FileId` and path and `SourceRoot` is constant. A file rename is
/// represented as a pair of deletion/creation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub u32);
