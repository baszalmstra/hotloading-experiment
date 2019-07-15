use crate::IrDatabase;
use mun_hir::{ApplicationTy, Ty, TypeCtor};
use inkwell::types::{AnyTypeEnum, VoidType};

pub(crate) fn ty_query(db: &impl IrDatabase, ty: Ty) -> AnyTypeEnum {
    let context = db.module().get_context();
    match ty {
        Ty::Empty => AnyTypeEnum::VoidType(context.void_type()),
        Ty::Apply(ApplicationTy {ctor, ..}) => match ctor {
            TypeCtor::Number => AnyTypeEnum::FloatType(context.f64_type()),
            TypeCtor::FnDef(_) => unreachable!("unknown type can not be converted"),
        },
        _ => unreachable!("unknown type can not be converted"),
    }
}