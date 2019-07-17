use crate::IrDatabase;
use inkwell::builder::Builder;
use inkwell::values::{AnyValueEnum, BasicValueEnum};
use inkwell::{
    module::{Linkage, Module},
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, VoidType},
    values::{BasicValue, FunctionValue, FloatMathValue},
};
use mun_hir::{ApplicationTy, Body, Expr, ExprId, FileId, Function, HirDatabase, InferenceResult, ModuleDef, Path, Resolver, Ty, TypeCtor, Resolution, BinaryOp};
use std::sync::Arc;

pub(crate) fn module_ir_query(db: &impl IrDatabase, file_id: FileId) -> Module {
    let module = db.context().create_module(&format!("module_{}", file_id.0));
    let mod_data = db.module_data(file_id);
    let definitions = mod_data.definitions();
    for def in definitions {
        match def {
            ModuleDef::Function(f) => function_ir_query(db, *f, &module),
            _ => {}
        }
    }

    module
}

pub(crate) fn function_ir_query(db: &impl IrDatabase, f: Function, module: &Module) {
    let name = f.name(db).to_string();
    let fun = if let AnyTypeEnum::FunctionType(ty) = db.type_ir(f.ty(db)) {
        module.add_function(&name, ty, None)
    } else {
        panic!("not a function type")
    };

    let builder = db.context().create_builder();
    let body_ir = fun.append_basic_block("body");
    builder.position_at_end(&body_ir);

    let code_gen = BodyIrGenerator::new(db, f, fun, builder);

    code_gen.gen_fn_body();
}

#[derive(Debug)]
struct BodyIrGenerator<'a, D: IrDatabase> {
    db: &'a D,
    body: Arc<Body>,
    infer: Arc<InferenceResult>,
    builder: Builder,
    fn_value: FunctionValue,
    hir: Function,
}

impl<'a, D: IrDatabase> BodyIrGenerator<'a, D> {
    fn new(db: &'a D, f: Function, fn_value: FunctionValue, builder: Builder) -> Self {
        let body = f.body(db);
        let infer = f.infer(db);

        BodyIrGenerator {
            db,
            body,
            infer,
            builder,
            fn_value,
            hir: f,
        }
    }

    fn gen_fn_body(&self) {
        let sig = self.db.fn_signature(self.hir);

        let ret_value = self.gen_expr(self.body.body_expr());
        if let Some(value) = ret_value {
            self.builder.build_return(Some(&value));
        } else {
            self.builder.build_return(None);
        }
    }

    fn gen_expr(&self, expr: ExprId) -> Option<inkwell::values::BasicValueEnum> {
        match &self.body[expr] {
            &Expr::Block { ref statements, tail } => tail.and_then(|expr| self.gen_expr(expr)),
            Expr::Path(ref p) => {
                let resolver = mun_hir::resolver_for_expr(self.body.clone(), self.db, expr);
                Some(self.gen_path_expr(p, expr, &resolver))
            }
            &Expr::BinaryOp {lhs, rhs, op} => {
                let lhs_value = self.gen_expr(lhs).expect("no lhs value");
                let rhs_value = self.gen_expr(rhs).expect( "no rhs value");
                match op.expect("missing op") {
                    BinaryOp::Add => Some(self.gen_add(lhs_value, rhs_value)),
                    BinaryOp::Subtract => Some(self.gen_sub(lhs_value, rhs_value)),
                    _ => unreachable!()
                }
            }
            _ => None,
        }
    }

    fn gen_path_expr(
        &self,
        path: &Path,
        expr: ExprId,
        resolver: &Resolver,
    ) -> inkwell::values::BasicValueEnum {
        let resolution = resolver
            .resolve_path_without_assoc_items(self.db, path)
            .take_values()
            .expect("unknown path");

        match resolution {
            Resolution::LocalBinding(pat) => self
                .body
                .params()
                .iter()
                .position(|p| *p == pat)
                .and_then(|i| self.fn_value.get_nth_param(i as u32))
                .expect("could not find pat"),
            Resolution::Def(_) => panic!("no support for module definitions"),
        }
    }

    fn gen_add(&self, lhs: BasicValueEnum, rhs: BasicValueEnum) -> BasicValueEnum {
        match lhs.get_type() {
            BasicTypeEnum::FloatType(_) => self.builder.build_float_add(*lhs.as_float_value(), *rhs.as_float_value(), "add").into(),
            BasicTypeEnum::IntType(_) => self.builder.build_int_add(*lhs.as_int_value(), *rhs.as_int_value(), "add").into(),
            _ => unreachable!()
        }
    }

    fn gen_sub(&self, lhs: BasicValueEnum, rhs: BasicValueEnum) -> BasicValueEnum {
        match lhs.get_type() {
            BasicTypeEnum::FloatType(_) => self.builder.build_float_sub(*lhs.as_float_value(), *rhs.as_float_value(), "add").into(),
            BasicTypeEnum::IntType(_) => self.builder.build_int_sub(*lhs.as_int_value(), *rhs.as_int_value(), "add").into(),
            _ => unreachable!()
        }
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

pub(crate) fn ty_ir_query(db: &impl IrDatabase, ty: Ty) -> AnyTypeEnum {
    let context = db.context();
    match ty {
        Ty::Empty => AnyTypeEnum::VoidType(context.void_type()),
        Ty::Apply(ApplicationTy { ctor, .. }) => match ctor {
            TypeCtor::Number => AnyTypeEnum::FloatType(context.f64_type()),
            TypeCtor::FnDef(f) => {
                let ty = db.fn_signature(f);
                let ret_ty: BasicTypeEnum =
                    try_convert_any_to_basic(db.type_ir(ty.ret().clone())).unwrap();
                let params: Vec<BasicTypeEnum> = ty
                    .params()
                    .iter()
                    .map(|p| try_convert_any_to_basic(db.type_ir(p.clone())).unwrap())
                    .collect();
                ret_ty.fn_type(&params, false).into()
            }
        },
        _ => unreachable!("unknown type can not be converted"),
    }
}

fn try_convert_any_to_basic(ty: AnyTypeEnum) -> Option<BasicTypeEnum> {
    match ty {
        AnyTypeEnum::ArrayType(t) => Some(t.into()),
        AnyTypeEnum::FloatType(t) => Some(t.into()),
        AnyTypeEnum::IntType(t) => Some(t.into()),
        AnyTypeEnum::PointerType(t) => Some(t.into()),
        AnyTypeEnum::StructType(t) => Some(t.into()),
        AnyTypeEnum::VectorType(t) => Some(t.into()),
        _ => None,
    }
}
