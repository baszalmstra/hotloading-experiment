//! HIR provides a high-level object oriented access to Mun code.

#[macro_use]
mod arena;
mod code_model;
mod db;
pub mod diagnostics;
mod display;
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
mod source_id;
mod ty;
mod type_ref;

pub use salsa;

pub use crate::{
    db::{
        DefDatabase, DefDatabaseStorage, HirDatabase, HirDatabaseStorage, RelativePathBuf,
        SourceDatabase, SourceDatabaseStorage,
    },
    display::HirDisplay,
    expr::{resolver_for_expr, BinaryOp, Body, Expr, ExprId, ExprScopes},
    ids::ItemLoc,
    input::{FileId, PackageInput},
    name::Name,
    name_resolution::PerNs,
    path::{Path, PathKind},
    raw::RawItems,
    resolve::{Resolution, Resolver},
    ty::{ApplicationTy, InferenceResult, Ty, TypeCtor},
};

use crate::{
    arena::{Arena, ArenaId, RawId},
    line_index::LineIndex,
    name::AsName,
    source_id::{AstId, AstIdMap, FileAstId},
};

pub use self::code_model::{FnData, Function, Module, ModuleDef};
