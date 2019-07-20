//! HIR provides a high-level object oriented access to Mun code.

#[macro_use]
mod arena;
mod source_id;
mod code_model;
mod db;
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
mod ty;
mod type_ref;
mod diagnostics;

pub use salsa;

pub use crate::{
    db::{
        DefDatabase, DefDatabaseStorage, HirDatabase, HirDatabaseStorage, SourceDatabase,
        SourceDatabaseStorage,
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
    source_id::{AstId, AstIdMap, FileAstId},
    line_index::LineIndex,
    name::AsName,
};

pub use self::code_model::{FnData, Function, Module, ModuleDef};
