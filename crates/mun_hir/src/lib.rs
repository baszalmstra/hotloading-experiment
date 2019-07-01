//! HIR provides a high-level object oriented access to Mun code.

#[macro_use]
mod arena;
mod ast_id;
mod code_model;
mod db;
mod ids;
mod input;
pub mod line_index;
mod model;
mod name;
mod raw;
mod path;
mod type_ref;
mod expr;

use mun_syntax::TreeArc;
pub use salsa;

pub use crate::{
    path::{Path, PathKind},
    db::{
        DefDatabase, DefDatabaseStorage, HirDatabase, HirDatabaseStorage, SourceDatabase,
        SourceDatabaseStorage,
    },
    ids::ItemLoc,
    input::{FileId, PackageInput},
    name::Name,
    raw::RawItems,
};

use crate::{
    arena::{Arena, ArenaId, RawId},
    ast_id::{AstId, AstIdMap, FileAstId},
    line_index::LineIndex,
    name::AsName,
};

pub use self::code_model::{FnData, Function, Module, ModuleDef};
