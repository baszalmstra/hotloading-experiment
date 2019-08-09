use super::ConstraintSystem;
use crate::arena::map::ArenaMap;
use crate::{Ty, ExprId, PatId};
use crate::arena::Arena;
use crate::ty::infer::constraints::{Constraint, ConstraintKind};
use std::rc::Rc;
use crate::ty::infer::TypeVarId;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry::Vacant;

#[derive(Clone, Debug)]
pub enum SolveResult {
    /// Could not find a solution
    Error,

    /// Could not find a solution with the given constraints
    NoSolution,

    /// Found a solution
    Solution(Solution)
}

#[derive(Clone, Debug)]
pub struct Solution {
    pub type_of_expr: ArenaMap<ExprId, Ty>,
    pub type_of_pat: ArenaMap<PatId, Ty>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum AllowedBindingKind {
    // Only the exact type is permitted
    Exact,

    // Supertypes of the specified type are permitted
    Supertypes,

    // Subtypes of the specified type are permitted
    Subtypes,
}

#[derive(Debug)]
struct PotentialBinding {
    // The type to which the type variable can be bound.
    binding_ty: Ty,

    // The kind of bindings that are permitted
    binding_kind: AllowedBindingKind,

    // The constraint that is the source of the binding
    source: Rc<Constraint>,
}

#[derive(Debug)]
struct PotentialBindings {
    variable: TypeVarId,
    bindings: Vec<PotentialBinding>,
}

impl PotentialBindings {
    pub fn new(var: TypeVarId) -> Self {
        PotentialBindings {
            variable: var,
            bindings: Vec::new()
        }
    }
}

impl ConstraintSystem {
    /// Solve the system of constraints.
    pub fn solve(&mut self) -> SolveResult {
        // Start by simplifying
        if !self.simplify() {
            return SolveResult::Error;
        }

        // If there are no more constraints we found a solution
        if self.constraints.is_empty() {
            return SolveResult::Solution(self.build_solution())
        }

        // Otherwise we'll have to guess at some type variable
        let result = self.solve_inner();

        result
    }

    pub fn solve_inner(&mut self) -> SolveResult {
        // Get the best variable binding we can do to get to a result.
        let best_binding = match self.determine_best_bindings() {
            Some(best_binding) => best_binding,
            None => return SolveResult::NoSolution
        };

        for binding in best_binding.bindings.iter() {
            // Construct a snapshot that we can roll back later
            let snapshot = self.snapshot();

            // Add the bind constraint
            self.constraints.push_back(Rc::new(Constraint {
                kind: ConstraintKind::Bind { a: Ty::Infer(best_binding.variable), b: binding.binding_ty.clone() },
                location: binding.source.location.clone()
            }));

            println!("-- Adding constraint {} binds to {:?}", best_binding.variable, binding.binding_ty.clone());

            let result = self.solve();

            // Roll back the snap shot
            self.rollback_to(snapshot);

            if let SolveResult::Solution(solution) = result {
                return SolveResult::Solution(solution)
            }
        }

        SolveResult::NoSolution
    }

    fn determine_best_bindings(&self) -> Option<PotentialBindings> {
        let mut cache = FxHashMap::default();
        let unsolved_type_variables = self.type_variables.borrow_mut().unsolved_variables();

        // Look for all potential type bindings
        for variable in unsolved_type_variables.iter() {
            let bindings = self.get_potential_bindings(*variable);
            if !bindings.bindings.is_empty() {
                cache.insert(variable, bindings);
            }
        };

        // TODO: Score the different bindings against each other
        // Return the first entry
        cache.into_iter().map(|(k,v)| v).next()
    }

    fn get_potential_bindings(&self, var: TypeVarId) -> PotentialBindings {
        let mut result = PotentialBindings::new(var);
        let mut types_seen = FxHashSet::default();

        // Gather the constraints associated with this type variable
        for potential_binding in self.gather_constraints_for_variable(var)
            .filter_map(|constraint| self.get_potential_binding_for_relational_constraint(var, constraint)) {
            let ty = potential_binding.binding_ty.clone();
            if types_seen.insert(ty) {
                result.bindings.push(potential_binding);
            };
        }

        result
    }

    fn get_potential_binding_for_relational_constraint(&self, var: TypeVarId, constraint: &Rc<Constraint>) -> Option<PotentialBinding> {
        match &constraint.kind {
            ConstraintKind::Convertible { from, to } => {
                let from = self.type_variables.borrow_mut().replace_if_possible(from);
                let to = self.type_variables.borrow_mut().replace_if_possible(to);
                let (kind, ty) = match (&*from, &*to) {
                    // If there is an error type involved, we cannot continue
                    (Ty::Unknown, _)
                    | (_, Ty::Unknown) => {
                        return None
                    }

                    (Ty::Infer(var), ty) => {
                        (AllowedBindingKind::Supertypes, ty)
                    },
                    (ty, Ty::Infer(var)) => {
                        (AllowedBindingKind::Subtypes, ty)
                    },
                    _ => return None
                };

                // Do not allow binding to another type variable
                if ty.is_type_variable() {
                    return None;
                }

                Some(PotentialBinding {
                    binding_kind: kind,
                    binding_ty: ty.clone(),
                    source: constraint.clone()
                })
            },
            ConstraintKind::Bind { .. }
            | ConstraintKind ::Equal { .. } => None
        }
    }

    fn gather_constraints_for_variable(&self, var: TypeVarId) -> impl Iterator<Item = &Rc<Constraint>> {
        self.constraints
            .iter()
            .filter(move |constraint| constraint.kind.references_variable(var))
    }

    fn build_solution(&self) -> Solution {
        let mut type_variables = self.type_variables.borrow_mut();
        Solution {
            type_of_expr: ArenaMap::from_iter(self.type_of_expr.iter().map(|(expr, ty)| {
                let ty = type_variables.replace_if_possible(ty).into_owned();
                (expr, ty)
            })),
            type_of_pat: ArenaMap::from_iter(self.type_of_pat.iter().map(|(pat, ty)| {
                let ty = type_variables.replace_if_possible(ty).into_owned();
                (pat, ty)
            }))
        }
    }
}