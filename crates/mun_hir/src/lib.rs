//! HIR provides a high-level object oriented access to Mun code.

#[macro_use]
mod arena;
mod ast_id;
mod code_model;
mod db;
mod expr;
mod ids;
mod input;
pub mod line_index;
mod model;
mod name;
mod name_resolution;
mod path;
mod raw;
mod resolve;
mod type_ref;
mod ty;
mod display;

use mun_syntax::TreeArc;
pub use salsa;

pub use crate::{
    db::{
        DefDatabase, DefDatabaseStorage, HirDatabase, HirDatabaseStorage, SourceDatabase,
        SourceDatabaseStorage,
    },
    display::{HirDisplay},
    expr::ExprScopes,
    ids::ItemLoc,
    input::{FileId, PackageInput},
    name::Name,
    name_resolution::PerNs,
    path::{Path, PathKind},
    raw::RawItems,
};

use crate::{
    arena::{Arena, ArenaId, RawId},
    ast_id::{AstId, AstIdMap, FileAstId},
    line_index::LineIndex,
    name::AsName,
};

pub use self::code_model::{FnData, Function, Module, ModuleDef};
