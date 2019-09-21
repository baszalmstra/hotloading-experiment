use mun_hir as hir;

use crate::ir::module::ModuleIR;
use crate::{Context, Module};
use inkwell::{types::AnyTypeEnum, OptimizationLevel};
use mun_target::spec::Target;
use std::sync::Arc;

#[salsa::query_group(IrDatabaseStorage)]
pub trait IrDatabase: hir::HirDatabase {
    #[salsa::input]
    fn context(&self) -> Arc<Context>;

    #[salsa::input]
    fn optimization_lvl(&self) -> OptimizationLevel;

    #[salsa::input]
    fn target(&self) -> Target;

    #[salsa::invoke(crate::ir::ty::ir_query)]
    fn type_ir(&self, ty: hir::Ty) -> AnyTypeEnum;

    #[salsa::invoke(crate::ir::module::ir_query)]
    fn module_ir(&self, file: hir::FileId) -> Arc<ModuleIR>;
}
