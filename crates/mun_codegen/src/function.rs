use inkwell::values::FunctionValue;
use crate::IrDatabase;
use mun_hir::{Function, HirDatabase};

pub(crate) fn function_query(db: &impl IrDatabase, f: Function) -> FunctionValue {
    let module = db.module();
    let context = module.get_context();
    let name = db.fn_data(f).name().to_string();
    let void_type = context.void_type();
    let fn_type = void_type.fn_type(&[], false);
    let fn_value = module.add_function(&name, fn_type, None);
    fn_value
}