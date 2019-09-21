use mun_hir as hir;

use crate::module::Module;
use inkwell::values::FunctionValue;
use inkwell::{context::Context, types::AnyTypeEnum};
use std::sync::Arc;

#[salsa::query_group(IrDatabaseStorage)]
pub trait IrDatabase: hir::HirDatabase {
    #[salsa::input]
    fn context(&self) -> Arc<Context>;

    #[salsa::invoke(crate::ir::ty_ir_query)]
    fn type_ir(&self, ty: hir::Ty) -> AnyTypeEnum;

    #[salsa::invoke(crate::ir::module_ir_query)]
    fn module_ir(&self, file: hir::FileId) -> Module;

    #[salsa::invoke(crate::code_gen::write_module_shared_object)]
    fn write_module_shared_object(&self, file: hir::FileId) -> bool;
}
