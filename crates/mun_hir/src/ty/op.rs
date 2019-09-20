use crate::{Ty, BinaryOp};

pub(super) fn binary_op_rhs_expectation(op: BinaryOp, lhs_ty: Ty) -> Ty {
    lhs_ty
}

pub(super) fn binary_op_return_ty(op: BinaryOp, rhs_ty: Ty) -> Ty {
    match op {
        BinaryOp::Add
        |BinaryOp::Subtract
        |BinaryOp::Divide
        |BinaryOp::Multiply
        |BinaryOp::Remainder
        |BinaryOp::Power => rhs_ty,

        BinaryOp::Assign
        |BinaryOp::AddAssign
        |BinaryOp::SubtractAssign
        |BinaryOp::DivideAssign
        |BinaryOp::MultiplyAssign
        |BinaryOp::RemainderAssign
        |BinaryOp::PowerAssign => Ty::Empty,
    }
}