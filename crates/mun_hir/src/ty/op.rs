use super::{Ty, TypeCtor, ApplicationTy, InferTy};
use crate::BinaryOp;

//pub(super) fn binary_op_rhs_expectation(op: BinaryOp, lhs_ty: Ty) -> Ty {
//    match op {
//        BinaryOp::Assign => match lhs_ty {
//            Ty::Float|Ty::Int => lhs_ty,
//            Ty::Infer(InferTy::IntVar(_)) => lhs_ty,
//            _ => Ty::Unknown
//        }
//        BinaryOp::Divide => match lhs_ty {
//            Ty::Float => lhs_ty,
//            Ty::Infer(_) => lhs_ty,
//            _ => Ty::Unknown
//        },
//        BinaryOp::Add
//        | BinaryOp::Subtract
//        | BinaryOp::Multiply => match lhs_ty {
//            Ty::Float | Ty::Int => lhs_ty,
//            Ty::Infer(_) => lhs_ty,
//            _ => Ty::Unknown
//        }
//        _ => Ty::Unknown
//    }
//}