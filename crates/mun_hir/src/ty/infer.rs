use crate::arena::map::ArenaMap;
use crate::code_model::src::HasSource;
use crate::code_model::DefWithBody;
use crate::diagnostics::{DiagnosticSink, UnresolvedValue};
use crate::expr::{Body, Expr, ExprId, Pat, PatId, Statement, Literal};
use crate::name_resolution::Namespace;
use crate::resolve::{Resolution, Resolver};
use crate::ty::infer::diagnostics::InferenceDiagnostic;
use crate::ty::lower::LowerDiagnostic;
use crate::ty::{Ty, TypableDef, type_variable};
use crate::type_ref::{TypeRef, TypeRefId};
use crate::{expr, FnData, Function, HirDatabase, Path, TypeCtor, BinaryOp, ApplicationTy, HirDisplay};
use mun_syntax::ast::{TypeRefKind, BinOp};
use mun_syntax::{ast, AstPtr};
use std::{mem, fmt};
use std::ops::Index;
use std::sync::Arc;
use ena::unify::{UnifyKey, InPlaceUnificationTable, UnifyValue, NoError};
use super::{op};
use std::borrow::{Cow, BorrowMut};
use std::cell::RefCell;
use crate::ty::type_variable::TypeVarId;
use super::{type_variable::TypeVariableTable};
use crate::display::HirFormatter;
use std::error::Error;
use std::collections::{LinkedList, VecDeque};
use std::iter::FromIterator;
use std::rc::Rc;

/// The result of type inference: A mapping from expressions and patterns to types.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct InferenceResult {
    type_of_expr: ArenaMap<ExprId, Ty>,
    type_of_pat: ArenaMap<PatId, Ty>,
    diagnostics: Vec<diagnostics::InferenceDiagnostic>,
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
    let (mut constraints, diagnostics,) = ConstraintGenerator::new(db, body.clone(), resolver).build();
    println!("-- Initial constraints");
    constraints.print(db, &body);

    println!("\n-- Constraints after simplification");
    constraints.simplify();
    constraints.print(db, &body);
    //let mut ctx = InferenceContext::new(db, body, resolver);

//    match def {
//        DefWithBody::Function(ref f) => ctx.collect_fn(),
//    }
//
//    ctx.infer_body();

//    Arc::new(ctx.resolve_all())
    unreachable!();
}

#[derive(Clone, Debug)]
struct Constraint {
    /// The type of the constraint
    kind: ConstraintKind,

    /// The location where this constraint applies
    location: ConstraintLocator
}

#[derive(Clone, Debug)]
enum ConstraintLocator {
    Expr(ExprId),
    Pat(PatId)
}

#[derive(Clone, Debug)]
enum ConstraintKind {
    /// The two types must be bound to the same type
    Equal { a: Ty, b: Ty },

    /// The first type is convertible to the second type
    Convertible { from: Ty, to: Ty },
}

#[derive(Clone, Debug)]
struct EqualConstraint {
    left: Ty,
    right: Ty,
}

#[derive(Debug, Clone)]
struct ConstraintGenerator<'a, D: HirDatabase> {
    db: &'a D,
    body: Arc<Body>,
    resolver: Resolver,

    diagnostics: Vec<InferenceDiagnostic>,

    type_of_pat: ArenaMap<PatId, Ty>,
    type_of_expr: ArenaMap<ExprId, Ty>,

    constraints: Vec<Constraint>,
    type_variables: RefCell<type_variable::TypeVariableTable>
}

impl<'a, D: HirDatabase> ConstraintGenerator<'a, D> {
    fn new(db: &'a D, body: Arc<Body>, resolver: Resolver) -> Self {
        ConstraintGenerator {
            db,
            body,
            resolver,

            diagnostics: Vec::new(),

            type_of_pat: ArenaMap::default(),
            type_of_expr: ArenaMap::default(),

            constraints: Vec::new(),
            type_variables: RefCell::new(TypeVariableTable::default())
        }
    }

    /// Given a `TypeRefId` construct a `Ty` by resolving the type reference. An diagnostic message
    /// will be emitted if no `Ty` could be constructed and `Ty::Unknown` is returned.
    fn make_ty(&mut self, type_ref: &TypeRefId) -> Ty {
        let result = Ty::from_hir(
            self.db,
            // FIXME use right resolver for block
            &self.resolver,
            &self.body.type_refs(),
            type_ref,
        );

        // Pass along any name resolution diagnostics to the inference result.
        for diag in result.diagnostics {
            let diag = match diag {
                LowerDiagnostic::UnresolvedType { id } => {
                    InferenceDiagnostic::UnresolvedType { id }
                }
            };
            self.diagnostics.push(diag);
        }

        result.ty
    }

    /// Constructs a new type variable that should resolve to a number.
    fn new_variable(&mut self) -> Ty {
        Ty::Infer(self.type_variables.borrow_mut().new_type_var())
    }

    /// Records that the given `pat` is associated the specified `ty`.
    fn write_pat_ty(&mut self, pat: PatId, ty: Ty) {
        self.type_of_pat.insert(pat, ty);
    }

    /// Records that the given `pat` is associated the specified `ty`.
    fn write_expr_ty(&mut self, expr: ExprId, ty: Ty) {
        self.type_of_expr.insert(expr, ty);
    }

    /// Adds a new constraint to the system
    fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    /// Visit the `expr` and collect constraints from the expression.
    fn visit_expr(&mut self, expr: ExprId) -> Ty {
        let body = self.body.clone();
        let ty = match &body[expr] {
            Expr::Missing => Ty::Unknown,
            Expr::Literal(l) => match l {
                Literal::Int(_) => Ty::Int,
                Literal::Float(_) => Ty::Float,
                _ => unreachable!()
            },
            Expr::Block { statements, tail} => self.visit_block(statements, tail),
            Expr::Path(p) => self.visit_path(expr, p),
//            Expr::UnaryOp { .. } => {},
//            Expr::BinaryOp { .. } => {},
            _ => unreachable!(),
        };
        self.write_expr_ty(expr, ty.clone());
        ty
    }

    fn visit_path(&mut self, tgt_expr: ExprId, p: &Path) -> Ty {
        // FIXME this could be more efficient...
        let resolver = expr::resolver_for_expr(self.body.clone(), self.db, tgt_expr);
        self.infer_path_expr(&resolver, p, tgt_expr.into())
            .unwrap_or(Ty::Unknown)
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

    fn visit_let(&mut self, pat: &PatId, type_ref: &Option<TypeRefId>, initializer: &Option<ExprId>) -> Ty {
        let ty = self.new_variable();

        // If there is a type specified, use that as the type of the binding
        match &type_ref.map(|tr| self.make_ty(&tr)) {
            Some(t) => {
                self.add_constraint(Constraint {
                    kind: ConstraintKind::Equal { a: ty.clone(), b: t.clone()},
                    location: ConstraintLocator::Pat(*pat)
                })
            }
            None => {}
        };

        // If there is an initializer it must be convertible to the binding
        match initializer {
            Some(initializer) => {
                let expr_ty = self.visit_expr(*initializer);
                self.add_constraint(Constraint {
                    kind: ConstraintKind::Convertible { from: expr_ty, to: ty.clone() },
                    location: ConstraintLocator::Pat(*pat)
                })
            }
            None => {}
        };

        self.write_pat_ty(*pat, ty.clone());
        ty
    }

    fn visit_block(&mut self, statements: &Vec<Statement>, tail: &Option<ExprId>) -> Ty {
        for statement in statements.iter() {
            match statement {
                Statement::Let { pat, type_ref, initializer} => self.visit_let(pat, type_ref, initializer),
                Statement::Expr(e) => self.visit_expr(*e),
            };
        }

        match tail {
            Some(expr) => self.visit_expr(*expr),
            None => Ty::Empty
        }
    }

    fn build(mut self) -> (ConstraintSystem, Vec<InferenceDiagnostic>) {
        let body = self.body.clone();
        let ret_type = self.make_ty(&body.ret_type());

        // The type of the body must be convertible to the actual return type of the function.
        let body_expr = self.visit_expr(body.body_expr());
        self.add_constraint(Constraint {
            kind: ConstraintKind::Convertible { from: body_expr, to: ret_type },
            location: ConstraintLocator::Expr(body.body_expr())
        });

        let constraints = ConstraintSystem {
            constraints: VecDeque::from_iter(self.constraints.into_iter().map(|c| Rc::new(c))),
            type_variables: self.type_variables,
            type_of_pat: self.type_of_pat,
            type_of_expr: self.type_of_expr
        };

        return (constraints, self.diagnostics);
    }
}

#[derive(Debug, Clone)]
struct ConstraintSystem {
    constraints: VecDeque<Rc<Constraint>>,
    type_variables: RefCell<type_variable::TypeVariableTable>,
    type_of_pat: ArenaMap<PatId, Ty>,
    type_of_expr: ArenaMap<ExprId, Ty>,
}

impl ConstraintSystem {
    pub fn print<'a, D:HirDatabase>(&self, db: &'a D, body: &Body) -> fmt::Result {
        // Print all patterns
        for (pat, ty) in self.type_of_pat.iter() {
            let ty = self.type_variables.borrow_mut().replace_if_possible(ty);
            match &body[pat] {
                Pat::Bind { name } => {
                    println!("{} := {}", name, ty.display(db));
                }
                _ => {}
            }
        }

        // Print all the constraints
        for constraint in self.constraints.iter() {
            match &constraint.kind {
                ConstraintKind::Equal { a, b } => {
                    let a = self.type_variables.borrow_mut().replace_if_possible(a);
                    let b = self.type_variables.borrow_mut().replace_if_possible(b);
                    println!("{} equals {}", a.display(db), b.display(db));
                },
                ConstraintKind::Convertible { from, to } => {
                    let from = self.type_variables.borrow_mut().replace_if_possible(from);
                    let to = self.type_variables.borrow_mut().replace_if_possible(to);
                    println!("{} is convertible to {}", from.display(db), to.display(db));
                }
            };
        }

        Ok(())
    }
}

enum SimplifyResult {
    /// Indicates that a simplification resulted in an error
    Error,

    /// Indicates that a simplification was successful.
    Solved,

    /// Indicates that the constraint still holds but did also not result in an error.
    Unresolved,
}

impl ConstraintSystem {
    /// Simplify the system of constraints, by breaking down complex constraints into simpler
    /// constraints.
    ///
    /// The result of the simplification is a constraint system consisting of only simple
    /// constraints relating type variables to each other or directly to fixed types. There are
    /// no constraints that involve type constructors on both sides.
    ///
    /// Returns false if an error occurred; true otherwise.
    pub fn simplify(&mut self) -> bool {
        let mut constraints = self.constraints.clone();
        while let Some(constraint) = constraints.pop_front() {
            match self.simplify_constraint(&constraint) {
                SimplifyResult::Error => {
                    self.constraints.retain(|c| &**c as *const Constraint != &*constraint as *const Constraint);
                    return false;
                },
                SimplifyResult::Solved => {
                    self.constraints.retain(|c| &**c as *const Constraint != &*constraint as *const Constraint);
                },
                SimplifyResult::Unresolved => {
                },
            }
        }

        true
    }

    pub fn simplify_constraint(&mut self, constraint: &Constraint) -> SimplifyResult {
        match &constraint.kind {
            ConstraintKind::Equal { a, b} => {
                let a = self.type_variables.borrow_mut().replace_if_possible(a);
                let b = self.type_variables.borrow_mut().replace_if_possible(b);
                if a == b {
                    return SimplifyResult::Solved;
                }

                match (&*a, &*b) {
                    // Unify if there are two type variables
                    (Ty::Infer(tv1), Ty::Infer(tv2)) => {
                        self.type_variables.borrow_mut().equate(*tv1, *tv2);
                        SimplifyResult::Solved
                    }

                    // Instantiate the type if only one is a type variable
                    (Ty::Infer(tv), other)
                    | (other, Ty::Infer(tv)) => {
                        self.type_variables.borrow_mut().instantiate(*tv, other.clone());
                        SimplifyResult::Solved
                    }

                    // Otherwise, the two types are different which is considered an error
                    (a,b) => SimplifyResult::Error
                }
            }
            ConstraintKind::Convertible { from, to} => {
                let from = self.type_variables.borrow_mut().replace_if_possible(from);
                let to = self.type_variables.borrow_mut().replace_if_possible(to);
                if from == to {
                    return SimplifyResult::Solved;
                }

                match (&*from, &*to) {
                    // If there are still type variables involved, forget it
                    (Ty::Infer(tv), ..) | (.., Ty::Infer(tv)) => SimplifyResult::Unresolved,

                    // List of possible conversions
                    (Ty::Int, Ty::Float) => SimplifyResult::Solved,

                    // Everything else is an error
                    _ => SimplifyResult::Error
                }
            }
        }
    }
}

//
//impl<'context, 'a, D:HirDatabase> TypeRelation for Equate<'context, 'a, D> {
//    fn relate(&mut self, a: Ty, b: Ty) -> Option<Ty> {
//        if a==b { return Some(a); }
//
//        let a = self.ctxt.var_unification_table.borrow_mut().resolve_ty_shallow(&a);
//        let b = self.ctxt.var_unification_table.borrow_mut().resolve_ty_shallow(&b);
//
//        match (*a, *b) {
//            (Ty::Unknown, ..)
//            | (.., Ty::Unknown) => Some(*a),
//
//            (Ty::Infer(InferTy::TypeVar(tv1)), Ty::Infer(InferTy::TypeVar(tv2)))
//            | (Ty::Infer(InferTy::IntVar(tv1)), Ty::Infer(InferTy::IntVar(tv2))) => {
//                self.ctxt.var_unification_table.borrow_mut().equate(tv1, tv2);
//                Some(*a)
//            }
//
//            (Ty::Infer(InferTy::TypeVar(tv)), other)
//            | (other, Ty::Infer(InferTy::TypeVar(tv))) => {
//                self.ctxt.var_unification_table.borrow_mut().instantiate(tv, other.clone());
//                Some(other)
//            }
//
//            (Ty::Infer(InferTy::IntVar(tv)), other@Ty::Int)
//            | (Ty::Infer(InferTy::IntVar(tv)), other@Ty::Float)
//            | (other@Ty::Int, Ty::Infer(InferTy::IntVar(tv)))
//            | (other@Ty::Float, Ty::Infer(InferTy::IntVar(tv))) => {
//                self.ctxt.var_unification_table.borrow_mut().instantiate(tv, other.clone());
//                Some(other)
//            }
//            _ => None,
//        }
//    }
//}
//
///// The inference context contains all information needed during type inference.
//#[derive(Clone, Debug)]
//struct InferenceContext<'a, D: HirDatabase> {
//    db: &'a D,
//    body: Arc<Body>,
//    resolver: Resolver,
//
//    type_of_expr: ArenaMap<ExprId, Ty>,
//    type_of_pat: ArenaMap<PatId, Ty>,
//    diagnostics: Vec<InferenceDiagnostic>,
//    var_unification_table: RefCell<TypeVariableTable>,
//
//    /// The return type of the function being inferred.
//    return_ty: Ty,
//}
//
//impl<'a, D: HirDatabase> InferenceContext<'a, D> {
//    fn new(db: &'a D, body: Arc<Body>, resolver: Resolver) -> Self {
//        InferenceContext {
//            type_of_expr: ArenaMap::default(),
//            type_of_pat: ArenaMap::default(),
//            diagnostics: Vec::default(),
//            var_unification_table: RefCell::new(TypeVariableTable::default()),
//            db,
//            body,
//            resolver,
//            return_ty: Ty::Unknown, // set in collect_fn_signature
//        }
//    }
//
//    /// Records that the given `expr` is associated the specified `ty`.
//    fn write_expr_ty(&mut self, expr: ExprId, ty: Ty) {
//        self.type_of_expr.insert(expr, ty);
//    }
//
//    /// Records that the given `pat` is associated the specified `ty`.
//    fn write_pat_ty(&mut self, pat: PatId, ty: Ty) {
//        self.type_of_pat.insert(pat, ty);
//    }
//
//    /// Record the types of the parameters and body of a function.
//    fn collect_fn(&mut self) {
//        let body = Arc::clone(&self.body); // avoid borrow checker problem
//        for (pat, type_ref) in body.params().iter() {
//            let ty = self.make_ty(type_ref);
//            self.infer_pat(*pat, &ty);
//        }
//        self.return_ty = self.make_ty(&body.ret_type())
//    }
//
//    /// Given a `TypeRefId` construct a `Ty` by resolving the type reference. An diagnostic message
//    /// will be emitted if no `Ty` could be constructed and `Ty::Unknown` is returned.
//    fn make_ty(&mut self, type_ref: &TypeRefId) -> Ty {
//        let result = Ty::from_hir(
//            self.db,
//            // FIXME use right resolver for block
//            &self.resolver,
//            &self.body.type_refs(),
//            type_ref,
//        );
//
//        // Pass along any name resolution diagnostics to the inference result.
//        for diag in result.diagnostics {
//            let diag = match diag {
//                LowerDiagnostic::UnresolvedType { id } => {
//                    InferenceDiagnostic::UnresolvedType { id }
//                }
//            };
//            self.diagnostics.push(diag);
//        }
//
//        result.ty
//    }
//
//    fn infer_pat(&mut self, pat: PatId, mut expected: &Ty) -> Ty {
//        let body = Arc::clone(&self.body); // avoid borrow checker problem
//        match &body[pat] {
//            Pat::Bind { .. } => {
//                let inner_ty = expected.clone();
//                self.write_pat_ty(pat, expected.clone());
//                inner_ty
//            }
//            _ => Ty::Unknown,
//        }
//    }
//
//    fn infer_body(&mut self) {
//        self.infer_expr(
//            self.body.body_expr(),
//            &Expectation::has_type(self.return_ty.clone()),
//        );
//    }
//
//    fn infer_expr(&mut self, tgt_expr: ExprId, expected: &Expectation) -> Ty {
//        let body = Arc::clone(&self.body); // avoid borrow checker problem
//        let mut ty = match &body[tgt_expr] {
//            Expr::Missing => Ty::Unknown,
//            Expr::Path(p) => {
//                // FIXME this could be more efficient...
//                let resolver = expr::resolver_for_expr(self.body.clone(), self.db, tgt_expr);
//                self.infer_path_expr(&resolver, p, tgt_expr.into())
//                    .unwrap_or(Ty::Unknown)
//            }
//            Expr::BinaryOp { lhs, rhs, op } => match op {
//                Some(op) => {
//                    let lhs_expectation = match op {
//                        _ => Expectation::none()
//                    };
//
//                    let lhs_ty = self.infer_expr(*lhs, &Expectation::none());
//                    let rhs_expectation = op::binary_op_rhs_expectation(*op, lhs_ty.clone());
//                    let rhs_ty = self.infer_expr(*rhs, &Expectation::none());
//                    let ty = self.infer_bin_expr(&lhs_ty, &rhs_ty, *op);
//                    match ty {
//                        None => {
//                            self.diagnostics.push(InferenceDiagnostic::CannotApplyBinaryOp { id: tgt_expr, lhs: lhs_ty, rhs: rhs_ty });
//                            Ty::Unknown
//                        },
//                        Some(ty) => ty
//                    }
//                }
//                _ => Ty::Unknown,
//            },
//            Expr::Block { statements, tail } => self.infer_block(statements, *tail, expected),
//            Expr::Literal(lit) => match lit {
//                Literal::String(_) => Ty::Unknown,
//                Literal::Bool(_) => Ty::Unknown,
//                Literal::Int(_) => self.var_unification_table.borrow_mut().new_integer_var(), // This might also still be used as a float
//                Literal::Float(_) => Ty::Float,
//            }
//            _ => Ty::Unknown,
//            //            Expr::Call { callee: _, args: _ } => {}
//            //            Expr::UnaryOp { expr: _, op: _ } => {}
//            //            Expr::Block { statements: _, tail: _ } => {}
//
//        };
//
//        let ty = if !self.unify(&ty, &expected.ty) {
//            self.diagnostics.push(InferenceDiagnostic::MismatchedTypes {
//                expected: expected.ty.clone(),
//                found: ty.clone(),
//                id: tgt_expr
//            });
//            ty
//        } else {
//            self.var_unification_table.borrow_mut().resolve_ty_as_possible(&mut vec![], ty)
//        };
//
////        let ty = if expected.ty != Ty::Unknown && ty != expected.ty {
////            match self.cast_implicit(&ty, &expected.ty) {
////                Some(ty) => ty,
////                None => {
////
////                    expected.ty.clone()
////                }
////            }
////        } else {
////            ty
////        };
//        self.write_expr_ty(tgt_expr, ty.clone());
//        ty
//    }
//
//    /// Resolves the type completely; type variables without known type are
//    /// replaced by Ty::Unknown.
//    fn resolve_ty_completely(&mut self, tv_stack: &mut Vec<TypeVarId>, ty: Ty) -> Ty {
//        ty.fold(&mut |ty| match ty {
//            Ty::Infer(tv) => {
//                let inner = tv.to_inner();
//                if tv_stack.contains(&inner) {
//                    // recursive type
//                    return tv.fallback_value();
//                }
//                if let Some(known_ty) = self.var_unification_table.probe_value(inner).known() {
//                    // known_ty may contain other variables that are known by now
//                    tv_stack.push(inner);
//                    let result = self.resolve_ty_completely(tv_stack, known_ty.clone());
//                    tv_stack.pop();
//                    result
//                } else {
//                    tv.fallback_value()
//                }
//            }
//            _ => ty,
//        })
//    }
//
//    fn can_cast_implicit(&self, from: &Ty, to: &Ty) -> bool {
//        match (from, to) {
//            (Ty::Int, Ty::Float) => true,
//            _ => false
//        }
//    }
//
//    fn infer_path_expr(&mut self, resolver: &Resolver, path: &Path, id: ExprOrPatId) -> Option<Ty> {
//        let resolution = match resolver
//            .resolve_path_without_assoc_items(self.db, path)
//            .take_values()
//        {
//            Some(resolution) => resolution,
//            None => {
//                self.diagnostics
//                    .push(InferenceDiagnostic::UnresolvedValue { id: id });
//                return None;
//            }
//        };
//
//        match resolution {
//            Resolution::LocalBinding(pat) => {
//                let ty = self.type_of_pat.get(pat)?.clone();
//                //let ty = self.resolve_ty_as_possible(&mut vec![], ty);
//                Some(ty)
//            }
//            Resolution::Def(def) => {
//                let typable: Option<TypableDef> = def.into();
//                let typable = typable?;
//                let mut ty = self.db.type_for_def(typable, Namespace::Values);
//                Some(ty)
//            }
//        }
//    }
//
//    fn infer_bin_expr(&mut self, lhs: &Ty, rhs: &Ty, op: BinaryOp) -> Option<Ty> {
//        match op {
//            BinOp::Add|BinOp::Subtract|BinOp::Multiply => match (lhs, rhs) {
//                (Ty::Float,Ty::Float)
//                | (Ty::Float, Ty::Int)
//                | (Ty::Float, Ty::Infer(InferTy::IntVar(_)))
//                | (Ty::Int,Ty::Float)
//                | (Ty::Infer(InferTy::IntVar(_)),Ty::Float) => {
//                    Some(Ty::Float)
//                }
//                (Ty::Int, Ty::Int)
//                | (Ty::Infer(InferTy::IntVar(_)), Ty::Int)
//                | (Ty::Int, Ty::Infer(InferTy::IntVar(_)))=> {
//                    Some(Ty::Int)
//                },
//                (Ty::Infer(InferTy::IntVar(_)), Ty::Infer(InferTy::IntVar(_))) => {
//                    Some(lhs.clone())
//                }
//                _ => None
//            },
//            BinOp::Divide => match (lhs, rhs) {
//                (Ty::Float,Ty::Float)
//                | (Ty::Float, Ty::Int)
//                | (Ty::Float, Ty::Infer(InferTy::IntVar(_)))
//                | (Ty::Int,Ty::Float)
//                | (Ty::Infer(InferTy::IntVar(_)),Ty::Float)
//                | (Ty::Int, Ty::Int)
//                | (Ty::Infer(InferTy::IntVar(_)), Ty::Int)
//                | (Ty::Int, Ty::Infer(InferTy::IntVar(_)))
//                | (Ty::Infer(InferTy::IntVar(_)), Ty::Infer(InferTy::IntVar(_))) => {
//                    Some(Ty::Float)
//                }
//                _ => None
//            },
//            _ => None
//        }
//    }
//
//    fn resolve_all(mut self) -> InferenceResult {
//        // FIXME resolve obligations as well (use Guidance if necessary)
//        let mut tv_stack = Vec::new();
//        let mut expr_types = mem::replace(&mut self.type_of_expr, ArenaMap::default());
//        for (expr, ty) in expr_types.iter_mut() {
//            let resolved = self.resolve_ty_completely(&mut tv_stack, mem::replace(ty, Ty::Unknown));
//            if *ty == Ty::Unknown {
//                self.report_expr_inference_failure(expr);
//            }
//            *ty = resolved;
//        }
//        let mut pat_types = mem::replace(&mut self.type_of_pat, ArenaMap::default());
//        dbg!(&pat_types);
//        for (pat, ty) in pat_types.iter_mut() {
//            let resolved = self.resolve_ty_completely(&mut tv_stack, mem::replace(ty, Ty::Unknown));
//            if *ty == Ty::Unknown {
//                self.report_pat_inference_failure(pat);
//            }
//            *ty = resolved;
//        }
//        dbg!(&pat_types);
//        InferenceResult {
//            //            method_resolutions: self.method_resolutions,
//            //            field_resolutions: self.field_resolutions,
//            //            variant_resolutions: self.variant_resolutions,
//            //            assoc_resolutions: self.assoc_resolutions,
//            type_of_expr: expr_types,
//            type_of_pat: pat_types,
//            diagnostics: self.diagnostics,
//        }
//    }
//
//    fn infer_block(
//        &mut self,
//        statements: &[Statement],
//        tail: Option<ExprId>,
//        expected: &Expectation,
//    ) -> Ty {
//        for stmt in statements {
//            match stmt {
//                Statement::Let {
//                    pat,
//                    type_ref,
//                    initializer,
//                } => {
//                    let decl_ty = type_ref
//                        .as_ref()
//                        .map(|tr| self.make_ty(tr))
//                        .unwrap_or(Ty::Unknown);
//                    let decl_ty = self.insert_type_vars(decl_ty);
//                    let ty = if let Some(expr) = initializer {
//                        let expr_ty = self.infer_expr(*expr, &Expectation::has_type(decl_ty));
//                        expr_ty
//                    } else {
//                        decl_ty
//                    };
//
//                    self.infer_pat(*pat, &ty);
//                }
//                Statement::Expr(expr) => {
//                    self.infer_expr(*expr, &Expectation::none());
//                }
//            }
//        }
//        let ty = if let Some(expr) = tail {
//            self.infer_expr(expr, expected)
//        } else {
//            Ty::Empty
//        };
//        ty
//    }
//
//    pub fn report_pat_inference_failure(&mut self, pat: PatId) {
//        //        self.diagnostics.push(InferenceDiagnostic::PatInferenceFailed {
//        //            pat
//        //        });
//    }
//
//    pub fn report_expr_inference_failure(&mut self, expr: ExprId) {
//        //        self.diagnostics.push(InferenceDiagnostic::ExprInferenceFailed {
//        //            expr
//        //        });
//    }
//}

/// The kinds of placeholders we need during type inference. There's separate
/// values for general types, and for integer and float variables. The latter
/// two are used for inference of literal values (e.g. `100` could be one of
/// several integer types).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum InferTy {
    TypeVar(TypeVarId),
    IntVar(TypeVarId),
}

impl InferTy {
    pub(super) fn to_inner(self) -> TypeVarId {
        match self {
            InferTy::TypeVar(ty) | InferTy::IntVar(ty) => ty,
        }
    }

    fn fallback_value(self) -> Ty {
        match self {
            InferTy::TypeVar(..) => Ty::Unknown,
            InferTy::IntVar(..) => Ty::Int,
        }
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

mod diagnostics {
    use crate::{code_model::src::HasSource, diagnostics::{DiagnosticSink, UnresolvedType, UnresolvedValue}, ty::infer::ExprOrPatId, type_ref::TypeRefId, Function, HirDatabase, Ty, ExprId, BinaryOp};
    use crate::diagnostics::{MismatchedType, CannotApplyBinaryOp};

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub(super) enum InferenceDiagnostic {
        UnresolvedValue { id: ExprOrPatId },
        UnresolvedType { id: TypeRefId },
        MismatchedTypes { id: ExprId, expected: Ty, found: Ty },
        CannotApplyBinaryOp { id:ExprId, lhs: Ty, rhs: Ty }
    }

    impl InferenceDiagnostic {
        pub(super) fn add_to(
            &self,
            db: &impl HirDatabase,
            owner: Function,
            sink: &mut DiagnosticSink,
        ) {
            match self {
                InferenceDiagnostic::UnresolvedValue { id } => {
                    let file = owner.source(db).file_id;
                    let body = owner.body_source_map(db);
                    let expr = match id {
                        ExprOrPatId::ExprId(id) => body.expr_syntax(*id),
                        ExprOrPatId::PatId(id) => {
                            body.pat_syntax(*id).map(|ptr| ptr.syntax_node_ptr())
                        }
                    }
                    .unwrap();

                    sink.push(UnresolvedValue { file, expr });
                }
                InferenceDiagnostic::UnresolvedType { id } => {
                    let file = owner.source(db).file_id;
                    let body = owner.body_source_map(db);
                    let type_ref = body.type_ref_syntax(*id).expect("If this is not found, it must be a type ref generated by the library which should never be unresolved.");
                    sink.push(UnresolvedType { file, type_ref });
                }
                InferenceDiagnostic::MismatchedTypes { id, found, expected } => {
                    let file = owner.source(db).file_id;
                    let body = owner.body_source_map(db);
                    let expr = body.expr_syntax(*id).unwrap();
                    sink.push(MismatchedType { file, expr, found: found.clone(), expected: expected.clone() });
                }
                InferenceDiagnostic::CannotApplyBinaryOp { id, lhs, rhs } => {
                    let file = owner.source(db).file_id;
                    let body = owner.body_source_map(db);
                    let expr = body.expr_syntax(*id).unwrap();
                    sink.push( CannotApplyBinaryOp { file, expr, lhs: lhs.clone(), rhs: rhs.clone() });
                }
            }
        }
    }
}