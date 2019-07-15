use mun_hir as hir;

use crate::module::Module;
use inkwell::values::FunctionValue;
use inkwell::{context::Context, types::AnyTypeEnum};
use std::sync::Arc;

#[salsa::query_group(IrDatabaseStorage)]
pub trait IrDatabase: hir::HirDatabase {
    #[salsa::input]
    fn module(&self) -> Arc<Module>;

    #[salsa::invoke(crate::ty::ty_ir_query)]
    fn type_ir(&self, ty: hir::Ty) -> AnyTypeEnum;

    #[salsa::invoke(crate::function::function_ir_query)]
    fn function_ir(&self, f: hir::Function) -> FunctionValue;
}
