use crate::arena::map::ArenaMap;
use crate::code_model::src::HasSource;
use crate::code_model::DefWithBody;
use crate::diagnostics::{DiagnosticSink, UnresolvedValue};
use crate::expr::{Body, Expr, ExprId, Pat, PatId, Statement};
use crate::name_resolution::Namespace;
use crate::resolve::{Resolution, Resolver};
use crate::ty::{Ty, TypableDef};
use crate::type_ref::TypeRef;
use crate::{expr, FnData, Function, HirDatabase, Path};
use mun_syntax::ast::TypeRefKind;
use mun_syntax::{ast, AstPtr};
use std::mem;
use std::ops::Index;
use std::sync::Arc;

/// The result of type inference: A mapping from expressions and patterns to types.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct InferenceResult {
    type_of_expr: ArenaMap<ExprId, Ty>,
    type_of_pat: ArenaMap<PatId, Ty>,
    diagnostics: Vec<InferenceDiagnostic>,
}

impl Index<ExprId> for InferenceResult {
    type Output = Ty;

    fn index(&self, expr: ExprId) -> &Ty {
        self.type_of_expr.get(expr).unwrap_or(&Ty::Unknown)
    }
}

impl Index<PatId> for InferenceResult {
    type Output = Ty;

    fn index(&self, pat: PatId) -> &Ty {
        self.type_of_pat.get(pat).unwrap_or(&Ty::Unknown)
    }
}

impl InferenceResult {
    pub(crate) fn add_diagnostics(
        &self,
        db: &impl HirDatabase,
        owner: Function,
        sink: &mut DiagnosticSink,
    ) {
        self.diagnostics
            .iter()
            .for_each(|it| it.add_to(db, owner, sink))
    }
}

/// The entry point of type inference.
pub fn infer_query(db: &impl HirDatabase, def: DefWithBody) -> Arc<InferenceResult> {
    let body = def.body(db);
    let resolver = def.resolver(db);
    let mut ctx = InferenceContext::new(db, body, resolver);

    match def {
        DefWithBody::Function(ref f) => ctx.collect_fn(&f.data(db)),
    }

    ctx.infer_body();

    Arc::new(ctx.resolve_all())
}

/// The inference context contains all information needed during type inference.
#[derive(Clone, Debug)]
struct InferenceContext<'a, D: HirDatabase> {
    db: &'a D,
    body: Arc<Body>,
    resolver: Resolver,

    type_of_expr: ArenaMap<ExprId, Ty>,
    type_of_pat: ArenaMap<PatId, Ty>,
    diagnostics: Vec<InferenceDiagnostic>,

    /// The return type of the function being inferred.
    return_ty: Ty,
}

impl<'a, D: HirDatabase> InferenceContext<'a, D> {
    fn new(db: &'a D, body: Arc<Body>, resolver: Resolver) -> Self {
        InferenceContext {
            type_of_expr: ArenaMap::default(),
            type_of_pat: ArenaMap::default(),
            diagnostics: Vec::default(),
            db,
            body,
            resolver,
            return_ty: Ty::Unknown, // set in collect_fn_signature
        }
    }

    fn write_expr_ty(&mut self, expr: ExprId, ty: Ty) {
        self.type_of_expr.insert(expr, ty);
    }

    fn write_pat_ty(&mut self, pat: PatId, ty: Ty) {
        self.type_of_pat.insert(pat, ty);
    }

    fn collect_fn(&mut self, data: &FnData) {
        let body = Arc::clone(&self.body); // avoid borrow checker problem
        for (type_ref, pat) in data.params().iter().zip(body.params()) {
            let ty = self.make_ty(type_ref).unwrap_or(Ty::Unknown);
            self.infer_pat(*pat, &ty);
        }
        self.return_ty = self.make_ty(data.ret_type()).unwrap_or(Ty::Unknown)
    }

    fn make_ty(&mut self, type_ref: &TypeRef) -> Option<Ty> {
        let ty = Ty::from_hir(
            self.db,
            // FIXME use right resolver for block
            &self.resolver,
            type_ref,
        )?;
        //self.insert_type_vars(ty)
        Some(ty)
    }

    fn infer_pat(&mut self, pat: PatId, mut expected: &Ty) -> Ty {
        let body = Arc::clone(&self.body); // avoid borrow checker problem
        match &body[pat] {
            Pat::Bind { .. } => {
                let inner_ty = expected.clone();
                self.write_pat_ty(pat, expected.clone());
                inner_ty
            }
            _ => Ty::Unknown,
        }
    }

    fn infer_body(&mut self) {
        self.infer_expr(
            self.body.body_expr(),
            &Expectation::has_type(self.return_ty.clone()),
        );
    }

    fn infer_expr(&mut self, tgt_expr: ExprId, expected: &Expectation) -> Ty {
        let body = Arc::clone(&self.body); // avoid borrow checker problem
        let ty = match &body[tgt_expr] {
            Expr::Missing => Ty::Unknown,
            Expr::Path(p) => {
                // FIXME this could be more efficient...
                let resolver = expr::resolver_for_expr(self.body.clone(), self.db, tgt_expr);
                self.infer_path_expr(&resolver, p, tgt_expr.into())
                    .unwrap_or(Ty::Unknown)
            }
            Expr::BinaryOp { lhs, rhs, op } => match op {
                Some(op) => {
                    let lhs_ty = self.infer_expr(*lhs, &Expectation::none());
                    let rhs_ty = self.infer_expr(*rhs, &Expectation::none());
                    lhs_ty
                }
                _ => Ty::Unknown,
            },
            Expr::Block { statements, tail } => self.infer_block(statements, *tail, expected),
            _ => Ty::Unknown,
            //            Expr::Call { callee: _, args: _ } => {}
            //            Expr::UnaryOp { expr: _, op: _ } => {}
            //            Expr::Block { statements: _, tail: _ } => {}
            //            Expr::Literal(_) => {}
        };
        self.write_expr_ty(tgt_expr, ty.clone());
        ty
    }

    fn infer_path_expr(&mut self, resolver: &Resolver, path: &Path, id: ExprOrPatId) -> Option<Ty> {
        let resolution = match resolver
            .resolve_path_without_assoc_items(self.db, path)
            .take_values()
        {
            Some(resolution) => resolution,
            None => {
                self.diagnostics
                    .push(InferenceDiagnostic::UnresolvedValue { id: id });
                return None;
            }
        };

        match resolution {
            Resolution::LocalBinding(pat) => {
                let ty = self.type_of_pat.get(pat)?.clone();
                //let ty = self.resolve_ty_as_possible(&mut vec![], ty);
                Some(ty)
            }
            Resolution::Def(def) => {
                let typable: Option<TypableDef> = def.into();
                let typable = typable?;
                let mut ty = self.db.type_for_def(typable, Namespace::Values);
                Some(ty)
            }
        }
    }

    fn resolve_all(mut self) -> InferenceResult {
        // FIXME resolve obligations as well (use Guidance if necessary)
        //let mut tv_stack = Vec::new();
        let mut expr_types = mem::replace(&mut self.type_of_expr, ArenaMap::default());
        for (expr, ty) in expr_types.iter_mut() {
            //let resolved = self.resolve_ty_completely(&mut tv_stack, mem::replace(ty, Ty::Unknown));
            if *ty == Ty::Unknown {
                self.report_expr_inference_failure(expr);
            }
            //*ty = resolved;
        }
        let mut pat_types = mem::replace(&mut self.type_of_pat, ArenaMap::default());
        for (pat, ty) in pat_types.iter_mut() {
            //let resolved = self.resolve_ty_completely(&mut tv_stack, mem::replace(ty, Ty::Unknown));
            if *ty == Ty::Unknown {
                self.report_pat_inference_failure(pat);
            }
            //*ty = resolved;
        }
        InferenceResult {
            //            method_resolutions: self.method_resolutions,
            //            field_resolutions: self.field_resolutions,
            //            variant_resolutions: self.variant_resolutions,
            //            assoc_resolutions: self.assoc_resolutions,
            type_of_expr: expr_types,
            type_of_pat: pat_types,
            diagnostics: self.diagnostics,
        }
    }

    fn infer_block(
        &mut self,
        statements: &[Statement],
        tail: Option<ExprId>,
        expected: &Expectation,
    ) -> Ty {
        for stmt in statements {
            match stmt {
                Statement::Let {
                    pat,
                    type_ref,
                    initializer,
                } => {
                    let decl_ty = type_ref
                        .as_ref()
                        .and_then(|tr| self.make_ty(tr))
                        .unwrap_or(Ty::Unknown);
                    //let decl_ty = self.insert_type_vars(decl_ty);
                    let ty = if let Some(expr) = initializer {
                        let expr_ty = self.infer_expr(*expr, &Expectation::has_type(decl_ty));
                        expr_ty
                    } else {
                        decl_ty
                    };

                    self.infer_pat(*pat, &ty);
                }
                Statement::Expr(expr) => {
                    self.infer_expr(*expr, &Expectation::none());
                }
            }
        }
        let ty = if let Some(expr) = tail {
            self.infer_expr(expr, expected)
        } else {
            Ty::Empty
        };
        ty
    }

    pub fn report_pat_inference_failure(&mut self, pat: PatId) {
        //        self.diagnostics.push(InferenceDiagnostic::PatInferenceFailed {
        //            pat
        //        });
    }

    pub fn report_expr_inference_failure(&mut self, expr: ExprId) {
        //        self.diagnostics.push(InferenceDiagnostic::ExprInferenceFailed {
        //            expr
        //        });
    }
}

/// When inferring an expression, we propagate downward whatever type hint we
/// are able in the form of an `Expectation`.
#[derive(Clone, PartialEq, Eq, Debug)]
struct Expectation {
    ty: Ty,
    // FIXME: In some cases, we need to be aware whether the expectation is that
    // the type match exactly what we passed, or whether it just needs to be
    // coercible to the expected type. See Expectation::rvalue_hint in rustc.
}

impl Expectation {
    /// The expectation that the type of the expression needs to equal the given
    /// type.
    fn has_type(ty: Ty) -> Self {
        Expectation { ty }
    }

    /// This expresses no expectation on the type.
    fn none() -> Self {
        Expectation { ty: Ty::Unknown }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(super) enum ExprOrPatId {
    ExprId(ExprId),
    PatId(PatId),
}

impl From<ExprId> for ExprOrPatId {
    fn from(e: ExprId) -> Self {
        ExprOrPatId::ExprId(e)
    }
}

impl From<PatId> for ExprOrPatId {
    fn from(p: PatId) -> Self {
        ExprOrPatId::PatId(p)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum InferenceDiagnostic {
    UnresolvedValue { id: ExprOrPatId },
}

impl InferenceDiagnostic {
    pub(super) fn add_to(&self, db: &impl HirDatabase, owner: Function, sink: &mut DiagnosticSink) {
        match self {
            InferenceDiagnostic::UnresolvedValue { id } => {
                let file = owner.source(db).file_id;
                let body = owner.body_source_map(db);
                let expr = match id {
                    ExprOrPatId::ExprId(id) => body.expr_syntax(*id),
                    ExprOrPatId::PatId(id) => body.pat_syntax(*id).map(|ptr| ptr.syntax_node_ptr()),
                }
                .unwrap();

                sink.push(UnresolvedValue { file, expr });
            }
        }
    }
}
