use crate::IrDatabase;
use inkwell::types::{AnyTypeEnum, BasicTypeEnum};
use inkwell::values::FunctionValue;
use mun_hir::{Function, HirDatabase, Ty};

pub(crate) fn function_ir_query(db: &impl IrDatabase, f: Function) -> FunctionValue {
    let module = db.module();
    let context = module.get_context();
    let name = f.name(db).to_string();
    if let AnyTypeEnum::FunctionType(ty) = db.type_ir(f.ty(db)) {
        module.add_function(&name, ty, None)
    } else {
        panic!("not a function type")
    }
}

fn as_parameter_type(db: &impl IrDatabase, ty: Ty) -> BasicTypeEnum {
    let any_type = db.type_ir(ty);
    match any_type {
        AnyTypeEnum::ArrayType(t) => BasicTypeEnum::ArrayType(t),
        AnyTypeEnum::FloatType(t) => BasicTypeEnum::FloatType(t),
        AnyTypeEnum::IntType(t) => BasicTypeEnum::IntType(t),
        AnyTypeEnum::PointerType(t) => BasicTypeEnum::PointerType(t),
        AnyTypeEnum::StructType(t) => BasicTypeEnum::StructType(t),
        AnyTypeEnum::VectorType(t) => BasicTypeEnum::VectorType(t),
        _ => unreachable!("not implemented"),
    }
}
