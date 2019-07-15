use mun_hir::{HirDatabase, Ty, Function};

use crate::module::Module;
use inkwell::{
    context::Context,
    types::AnyTypeEnum
};
use inkwell::values::FunctionValue;
use std::sync::Arc;

#[salsa::query_group(IrDatabaseStorage)]
pub trait IrDatabase: HirDatabase {
    #[salsa::input]
    fn module(&self) -> Arc<Module>;

    #[salsa::invoke(crate::ty::ty_query)]
    fn ir_type(&self, ty: Ty) -> AnyTypeEnum;

    #[salsa::invoke(crate::function::function_query)]
    fn ir_function(&self, f: Function) -> FunctionValue;
}