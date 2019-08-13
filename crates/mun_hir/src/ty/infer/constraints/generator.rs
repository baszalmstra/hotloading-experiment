use super::{ConstraintKind, ConstraintLocator, ConstraintSystem, Constraint, NumberType};
use std::{
    cell::RefCell,
    sync::Arc,
    collections::VecDeque,
    rc::Rc,
    iter::FromIterator
};
use crate::{
    ty::infer::ExprOrPatId,
    ty::lower::LowerDiagnostic,
    type_ref::TypeRefId,
    arena::map::ArenaMap,
    HirDatabase,
    Body,
    Resolver,
    PatId,
    ExprId,
    Ty,
    Expr,
    expr,
    Path,
    Resolution,
    Statement,
    name_resolution::Namespace,
    ty::TypableDef,
    Literal,
    ty::infer::diagnostics::{InferenceDiagnostic}
};
use super::type_variable::TypeVariableTable;
use crate::expr::BinaryOp;
use crate::arena::Arena;
use rustc_hash::FxHashMap;

pub(crate) struct ConstraintGenerator<'a, D: HirDatabase> {
    db: &'a D,
    body: Arc<Body>,
    resolver: Resolver,

    diagnostics: Vec<InferenceDiagnostic>,

    type_of_pat: ArenaMap<PatId, Ty>,
    type_of_expr: ArenaMap<ExprId, Ty>,

    constraints: Vec<Constraint>,
    type_variables: RefCell<TypeVariableTable>,
}

impl<'a, D: HirDatabase> ConstraintGenerator<'a, D> {
    pub fn new(db: &'a D, body: Arc<Body>, resolver: Resolver) -> Self {
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
                Literal::Int(_) => {
                    let var = self.new_variable();
                    self.constraints.push(Constraint {
                        kind: ConstraintKind::NumberLiteral { ty: var.clone(), number_ty: NumberType::Integer },
                        location: ConstraintLocator::Expr(expr)
                    });
                    var
                }
                Literal::Float(_) => Ty::Float,
                _ => unreachable!()
            },
            Expr::Block { statements, tail} => self.visit_block(expr, statements, tail),
            Expr::Path(p) => self.visit_path(expr, p),
//            Expr::UnaryOp { .. } => {},
            Expr::BinaryOp { lhs, rhs, op } => {
                let lhs_ty = self.visit_expr(*lhs);
                let rhs_ty = self.visit_expr(*rhs);
                match op {
                    Some(BinaryOp::Assign) => {
                        self.add_constraint(Constraint {
                            kind: ConstraintKind::Equal { a: rhs_ty, b: lhs_ty },
                            location: ConstraintLocator::Expr(expr)
                        });
                        Ty::Empty
                    }
                    _ => {
                        self.diagnostics.push(InferenceDiagnostic::CannotApplyBinaryOp {
                            id: expr,
                            lhs: lhs_ty,
                            rhs: rhs_ty
                        });
                        Ty::Unknown
                    }
                }
            },
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
                let ty = self.db.type_for_def(typable, Namespace::Values);
                Some(ty)
            }
        }
    }

    fn visit_let(&mut self, pat: &PatId, type_ref: &Option<TypeRefId>, initializer: &Option<ExprId>) -> Ty {
        // If there is a type specified, use that as the type of the binding.
        let ty = match type_ref.map(|tr| self.make_ty(&tr)) {
            // If no type was specified introduce a type variable
            None => self.new_variable(),

            // If the type is unknown (because the TypeRef is unknown), also introduce a new
            // type variable
            Some(Ty::Unknown) => self.new_variable(),

            // Otherwise, just use the assigned type
            Some(ty) => ty
        };

        // If there is an initializer it must be convertible to the binding
        match initializer {
            Some(initializer) => {
                let expr_ty = self.visit_expr(*initializer);
                self.add_constraint(Constraint {
                    kind: ConstraintKind::Equal { a: ty.clone(), b: expr_ty },
                    location: ConstraintLocator::Pat(*pat)
                })
            }
            None => {}
        };

        self.write_pat_ty(*pat, ty.clone());
        ty
    }

    fn visit_block(&mut self, block_expr: ExprId, statements: &Vec<Statement>, tail: &Option<ExprId>) -> Ty {
        for statement in statements.iter() {
            match statement {
                Statement::Let { pat, type_ref, initializer} => self.visit_let(pat, type_ref, initializer),
                Statement::Expr(e) => self.visit_expr(*e),
            };
        }

        match tail {
            Some(expr) => {
                let tail_ty = self.visit_expr(*expr);

                if self.body.body_expr() == block_expr {
                    let block_ty = self.new_variable();
                    self.add_constraint(Constraint {
                        kind: ConstraintKind::Convertible { from: tail_ty, to: block_ty.clone() },
                        location: ConstraintLocator::Expr(*expr)
                    });
                    block_ty
                } else {
                    tail_ty
                }
            },
            None => Ty::Empty
        }
    }

    pub(super) fn build(mut self) -> (ConstraintSystem, Vec<InferenceDiagnostic>) {
        let body = self.body.clone();

        // Add arguments
        for (pat, ty) in body.params().iter() {
            let ty = self.make_ty(ty);
            self.write_pat_ty(*pat, ty);
        }

        // The type of the body must be convertible to the actual return type of the function.
        let ret_type = self.make_ty(&body.ret_type());
        let body_expr = self.visit_expr(body.body_expr());
        self.add_constraint(Constraint {
            kind: ConstraintKind::Equal { a: body_expr, b: ret_type },
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