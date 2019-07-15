use crate::IrDatabase;
use inkwell::types::{AnyTypeEnum, VoidType, BasicTypeEnum, BasicType};
use mun_hir::{ApplicationTy, Ty, TypeCtor};
use std::convert::TryInto;

pub(crate) fn ty_ir_query(db: &impl IrDatabase, ty: Ty) -> AnyTypeEnum {
    let context = db.module().get_context();
    match ty {
        Ty::Empty => AnyTypeEnum::VoidType(context.void_type()),
        Ty::Apply(ApplicationTy { ctor, .. }) => match ctor {
            TypeCtor::Number => AnyTypeEnum::FloatType(context.f64_type()),
            TypeCtor::FnDef(f) => {
                let ty = db.fn_signature(f);
                let ret_ty:BasicTypeEnum = try_convert_any_to_basic(db.type_ir(ty.ret().clone())).unwrap();
                let params:Vec<BasicTypeEnum> = ty.params().iter().map(|p| try_convert_any_to_basic(db.type_ir(p.clone())).unwrap()).collect();
                ret_ty.fn_type(&params, false).into()
            }
        },
        _ => unreachable!("unknown type can not be converted"),
    }
}

fn try_convert_any_to_basic(ty:AnyTypeEnum) -> Option<BasicTypeEnum> {
    match ty {
        AnyTypeEnum::ArrayType(t) => Some(t.into()),
        AnyTypeEnum::FloatType(t) => Some(t.into()),
        AnyTypeEnum::IntType(t) => Some(t.into()),
        AnyTypeEnum::PointerType(t) => Some(t.into()),
        AnyTypeEnum::StructType(t) => Some(t.into()),
        AnyTypeEnum::VectorType(t) => Some(t.into()),
        _ => None
    }
}