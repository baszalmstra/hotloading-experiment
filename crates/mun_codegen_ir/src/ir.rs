use crate::IrDatabase;
use inkwell::builder::Builder;
use inkwell::values::{AnyValueEnum, BasicValueEnum};
use inkwell::{
    module::{Linkage, Module},
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, VoidType},
    values::{BasicValue, FloatMathValue, FunctionValue},
};
use mun_hir::{ApplicationTy, BinaryOp, Body, Expr, Statement, ExprId, PatId, FileId, Function, HirDatabase, InferenceResult, ModuleDef, Path, Resolution, Resolver, Ty, TypeCtor, Pat};
use std::sync::Arc;
use std::collections::HashMap;
use inkwell::passes::PassManager;

fn create_function_pass_manager(module: &Module) -> PassManager {
    let function_pass_manager = PassManager::create_for_function(module);

    // Promote allocas to registers.
    function_pass_manager.add_promote_memory_to_register_pass();

    // Do simple "peephole" optimizations and bit-twiddling optzns.
    function_pass_manager.add_instruction_combining_pass();

    // Reassociate expressions.
    function_pass_manager.add_reassociate_pass();

    // Eliminate Common SubExpressions.
    function_pass_manager.add_gvn_pass();

    // Simplify the control flow graph (deleting unreachable blocks, etc).
    function_pass_manager.add_cfg_simplification_pass();

    function_pass_manager.initialize();

    function_pass_manager
}

pub(crate) fn module_ir_query(db: &impl IrDatabase, file_id: FileId) -> Module {
    let module = db
        .context()
        .create_module(db.file_relative_path(file_id).as_str());

    let fn_pass_manager = create_function_pass_manager(&module);

    let mod_data = db.module_data(file_id);
    let definitions = mod_data.definitions();
    for def in definitions {
        match def {
            ModuleDef::Function(f) => {
                let fun = function_ir_query(db, *f, &module);
                fn_pass_manager.run_on_function(&fun);
            },
            _ => {}
        }
    }

    module
}

pub(crate) fn function_ir_query(db: &impl IrDatabase, f: Function, module: &Module) -> FunctionValue {
    let name = f.name(db).to_string();
    let fun = if let AnyTypeEnum::FunctionType(ty) = db.type_ir(f.ty(db)) {
        module.add_function(&name, ty, None)
    } else {
        panic!("not a function type")
    };

    let builder = db.context().create_builder();
    let body_ir = fun.append_basic_block("body");
    builder.position_at_end(&body_ir);

    let mut code_gen = BodyIrGenerator::new(db, f, fun, builder);

    code_gen.gen_fn_body();

    fun
}

#[derive(Debug)]
struct BodyIrGenerator<'a, D: IrDatabase> {
    db: &'a D,
    body: Arc<Body>,
    infer: Arc<InferenceResult>,
    builder: Builder,
    fn_value: FunctionValue,
    hir: Function,
    pat_to_param: HashMap<PatId, inkwell::values::BasicValueEnum>,
    pat_to_local: HashMap<PatId, inkwell::values::PointerValue>,
    pat_to_name: HashMap<PatId, String>,
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
            pat_to_param: HashMap::default(),
            pat_to_local: HashMap::default(),
            pat_to_name: HashMap::default()
        }
    }

    fn gen_fn_body(&mut self) {
        let sig = self.db.fn_signature(self.hir);

        for (i, (pat, ty)) in self.body.params().iter().enumerate() {
            let body = self.body.clone(); // Avoid borrow issues
            match &body[*pat] {
                Pat::Bind { name } => {
                    let name = name.to_string();
                    let param =  self.fn_value.get_nth_param(i as u32).unwrap();
                    try_set_name(param.into(), &name);
                    self.pat_to_param.insert(*pat, param);
                    self.pat_to_name.insert(*pat, name);
                }
                Pat::Wild => {},
                Pat::Missing | Pat::Path(_) => unreachable!()
            }
        }

        let ret_value = self.gen_expr(self.body.body_expr());
        if let Some(value) = ret_value {
            self.builder.build_return(Some(&value));
        } else {
            self.builder.build_return(None);
        }
    }

    fn gen_expr(&mut self, expr: ExprId) -> Option<inkwell::values::BasicValueEnum> {
        let body = self.body.clone();
        match &body[expr] {
            &Expr::Block {
                ref statements,
                tail,
            } => {
                for statement in statements.iter() {
                    match statement {
                        Statement::Let { pat, initializer, .. } => {
                            self.gen_let_statement(pat, initializer);
                        },
                        Statement::Expr(expr) => { self.gen_expr(*expr); },
                    };
                };
                tail.and_then(|expr| self.gen_expr(expr))
            },
            Expr::Path(ref p) => {
                let resolver = mun_hir::resolver_for_expr(self.body.clone(), self.db, expr);
                Some(self.gen_path_expr(p, expr, &resolver))
            }
            &Expr::BinaryOp { lhs, rhs, op } => match op.expect("missing op") {
                BinaryOp::Add => Some(self.gen_add(lhs, rhs)),
                BinaryOp::Subtract => Some(self.gen_sub(lhs, rhs)),
                BinaryOp::Divide => Some(self.gen_divide(lhs, rhs)),
                BinaryOp::Multiply => Some(self.gen_multiply(lhs, rhs)),
//                BinaryOp::Remainder => Some(self.gen_remainder(lhs, rhs)),
//                BinaryOp::Power =>,
//                BinaryOp::Assign,
//                BinaryOp::AddAssign,
//                BinaryOp::SubtractAssign,
//                BinaryOp::DivideAssign,
//                BinaryOp::MultiplyAssign,
//                BinaryOp::RemainderAssign,
//                BinaryOp::PowerAssign,
                _ => unreachable!(),
            },
            _ => None,
        }
    }

    fn new_alloca_builder(&self) -> Builder {
        let temp_builder = Builder::create();
        let block = self.builder.get_insert_block().expect("at this stage there must be a block");
        if let Some(first_instruction) = block.get_first_instruction() {
            temp_builder.position_before(&first_instruction);
        } else {
            temp_builder.position_at_end(&block);
        }
        temp_builder
    }

    fn gen_let_statement(&mut self, pat: &PatId, initializer: &Option<ExprId>) {
        let initializer = initializer.and_then(|expr| self.gen_expr(expr));

        match &self.body[*pat] {
            Pat::Bind { name } => {
                let builder = self.new_alloca_builder();
                let ty = try_convert_any_to_basic(self.db.type_ir(self.infer[*pat].clone())).expect("expected basic type");
                let ptr = builder.build_alloca(ty, &name.to_string());
                self.pat_to_local.insert(*pat, ptr);
                self.pat_to_name.insert(*pat, name.to_string());
                if let Some(value) = initializer {
                    self.builder.build_store(ptr, value);
                };
            }
            Pat::Wild => {},
            Pat::Missing | Pat::Path(_) => unreachable!()
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
            Resolution::LocalBinding(pat) => {
                if let Some(param) = self.pat_to_param.get(&pat) {
                    *param
                } else if let Some(ptr) = self.pat_to_local.get(&pat) {
                    let name = self.pat_to_name.get(&pat).expect("could not find pat name");
                    self.builder.build_load(*ptr, &name)
                } else {
                    unreachable!("could not find the pattern..");
                }
            },
            Resolution::Def(_) => panic!("no support for module definitions"),
        }
    }

    fn gen_add(&mut self, lhs: ExprId, rhs: ExprId) -> BasicValueEnum {
        let lhs_value = self.gen_expr(lhs).expect("no lhs value");
        let rhs_value = self.gen_expr(rhs).expect("no rhs value");
        let lhs_type = &self.infer[lhs];
        let rhs_type = &self.infer[rhs];

        match (lhs_type, rhs_type) {
            (
                Ty::Apply(ApplicationTy {
                    ctor: TypeCtor::Number,
                    ..
                }),
                Ty::Apply(ApplicationTy {
                    ctor: TypeCtor::Number,
                    ..
                }),
            ) => self
                .builder
                .build_float_add(
                    *lhs_value.as_float_value(),
                    *rhs_value.as_float_value(),
                    "add",
                )
                .into(),
            _ => unreachable!(),
        }
    }

    fn gen_sub(&mut self, lhs: ExprId, rhs: ExprId) -> BasicValueEnum {
        let lhs_value = self.gen_expr(lhs).expect("no lhs value");
        let rhs_value = self.gen_expr(rhs).expect("no rhs value");
        let lhs_type = &self.infer[lhs];
        let rhs_type = &self.infer[rhs];

        match (lhs_type, rhs_type) {
            (
                Ty::Apply(ApplicationTy {
                    ctor: TypeCtor::Number,
                    ..
                }),
                Ty::Apply(ApplicationTy {
                    ctor: TypeCtor::Number,
                    ..
                }),
            ) => self
                .builder
                .build_float_sub(
                    *lhs_value.as_float_value(),
                    *rhs_value.as_float_value(),
                    "sub",
                )
                .into(),
            _ => unreachable!(),
        }
    }

    fn gen_multiply(&mut self, lhs: ExprId, rhs: ExprId) -> BasicValueEnum {
        let lhs_value = self.gen_expr(lhs).expect("no lhs value");
        let rhs_value = self.gen_expr(rhs).expect("no rhs value");
        let lhs_type = &self.infer[lhs];
        let rhs_type = &self.infer[rhs];

        match (lhs_type, rhs_type) {
            (
                Ty::Apply(ApplicationTy {
                              ctor: TypeCtor::Number,
                              ..
                          }),
                Ty::Apply(ApplicationTy {
                              ctor: TypeCtor::Number,
                              ..
                          }),
            ) => self
                .builder
                .build_float_mul(
                    *lhs_value.as_float_value(),
                    *rhs_value.as_float_value(),
                    "sub",
                )
                .into(),
            _ => unreachable!(),
        }
    }

    fn gen_divide(&mut self, lhs: ExprId, rhs: ExprId) -> BasicValueEnum {
        let lhs_value = self.gen_expr(lhs).expect("no lhs value");
        let rhs_value = self.gen_expr(rhs).expect("no rhs value");
        let lhs_type = &self.infer[lhs];
        let rhs_type = &self.infer[rhs];

        match (lhs_type, rhs_type) {
            (
                Ty::Apply(ApplicationTy {
                              ctor: TypeCtor::Number,
                              ..
                          }),
                Ty::Apply(ApplicationTy {
                              ctor: TypeCtor::Number,
                              ..
                          }),
            ) => self
                .builder
                .build_float_div(
                    *lhs_value.as_float_value(),
                    *rhs_value.as_float_value(),
                    "sub",
                )
                .into(),
            _ => unreachable!(),
        }
    }
}

/// Given a mun type, construct an LLVM IR type
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

fn try_set_name(ty: AnyValueEnum, name: &str) {
    match ty {
        AnyValueEnum::ArrayValue(v) => v.set_name(name),
        AnyValueEnum::IntValue(v) => v.set_name(name),
        AnyValueEnum::FloatValue(v) => v.set_name(name),
        AnyValueEnum::PhiValue(v) => {},
        AnyValueEnum::FunctionValue(v) => {},
        AnyValueEnum::PointerValue(v) => v.set_name(name),
        AnyValueEnum::StructValue(v) => v.set_name(name),
        AnyValueEnum::VectorValue(v) => v.set_name(name),
        AnyValueEnum::InstructionValue(v) => {},
    }
}