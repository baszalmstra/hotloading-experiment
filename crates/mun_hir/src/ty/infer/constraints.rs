//! This module implements the constraint system that is used by Mun to infer expression types.

use super::{type_variable};
use crate::{
    arena::map::ArenaMap, ty::infer::diagnostics::InferenceDiagnostic, Body, ExprId, HirDatabase,
    HirDisplay, Pat, PatId, Resolver, Ty,
};
use std::{cell::RefCell, collections::VecDeque, fmt, rc::Rc, sync::Arc};
use drop_bomb::DropBomb;

mod generator;
mod simplify;
mod snapshot;
mod solve;

use crate::ty::infer::TypeVarId;

#[derive(Clone, Debug)]
struct Constraint {
    /// The type of the constraint
    kind: ConstraintKind,

    /// The location where this constraint applies
    location: ConstraintLocator,
}

#[derive(Clone, Debug)]
enum ConstraintLocator {
    Expr(ExprId),
    Pat(PatId),
}

#[derive(Clone, Debug)]
enum ConstraintKind {
    /// The two types must be bound to the same type
    Equal { a: Ty, b: Ty },

    /// The first type is convertible to the second type
    Convertible { from: Ty, to: Ty },
}

impl ConstraintKind {
    /// Returns true if the specified variable is directly mentioned in the constraint.
    pub fn references_variable(&self, var:TypeVarId) -> bool {
        match &self {
            ConstraintKind::Equal { a: Ty::Infer(var), b: _ }
            | ConstraintKind::Equal { a: _, b: Ty::Infer(var) }
            | ConstraintKind::Convertible { from: Ty::Infer(var), to: _}
            | ConstraintKind::Convertible { from: _, to: Ty::Infer(var)} => true,
            _ => false,
        }
    }
}

pub(crate) struct ConstraintSystem {
    constraints: VecDeque<Rc<Constraint>>,
    type_variables: RefCell<type_variable::TypeVariableTable>,
    type_of_pat: ArenaMap<PatId, Ty>,
    type_of_expr: ArenaMap<ExprId, Ty>,
}

impl ConstraintSystem {
    pub(super) fn from_body<D: HirDatabase>(
        db: &D,
        body: Arc<Body>,
        resolver: Resolver,
    ) -> (ConstraintSystem, Vec<InferenceDiagnostic>) {
        generator::ConstraintGenerator::new(db, body, resolver).build()
    }
}

impl ConstraintSystem {
    pub fn print<'a, D: HirDatabase>(&self, db: &'a D, body: &Body) -> fmt::Result {
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