use super::ConstraintSystem;
use crate::arena::map::ArenaMap;
use crate::{Ty, ExprId, PatId};
use crate::arena::Arena;
use crate::ty::infer::constraints::{Constraint, ConstraintKind, NumberType};
use std::rc::Rc;
use crate::ty::infer::TypeVarId;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry::Vacant;

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
    pub fn solve(&mut self, solutions: &mut Vec<Solution>) -> bool {
        // Start by simplifying
        if !self.simplify() {
            return false;
        }

        // If there are no more constraints we found a solution
        if self.constraints.is_empty() {
            solutions.push(self.build_solution());
            return true;
        }

        // Otherwise we'll have to guess at some type variable
        self.solve_inner(solutions)
    }

    pub fn solve_inner(&mut self, solutions: &mut Vec<Solution>) -> bool {
         for bindings in self.determine_best_bindings() {
            for binding in bindings.bindings.iter() {
                // Construct a snapshot that we can roll back later
                let snapshot = self.snapshot();

                // Add the bind constraint
                self.constraints.push_back(Rc::new(Constraint {
                    kind: ConstraintKind::Equal { a: Ty::Infer(bindings.variable), b: binding.binding_ty.clone() },
                    location: binding.source.location.clone()
                }));

                println!("-- Adding constraint {} binds to {:?} {:?}", bindings.variable, binding.binding_ty.clone(), binding.source);

                if !self.solve(solutions) {
                    println!("--   Failed");
                }

                // Roll back the snap shot
                self.rollback_to(snapshot);
            }
        }

        true
    }

    fn determine_best_bindings(&self) -> Vec<PotentialBindings> {
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
        cache.into_iter().map(|(v, binding)| binding).collect()
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
            ConstraintKind::NumberLiteral { ty: ty, number_ty } => Some(PotentialBinding {
                binding_ty: match number_ty {
                    NumberType::Float => Ty::Float,
                    NumberType::Integer => Ty::Int,
                },
                source: constraint.clone(),
                binding_kind: AllowedBindingKind::Exact,
            }),
            ConstraintKind ::Equal { .. } => None
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