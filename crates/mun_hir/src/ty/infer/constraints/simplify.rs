use super::{ConstraintSystem, Constraint, ConstraintKind};
use crate::Ty;
use crate::ty::infer::constraints::NumberType;

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
        loop {
            let mut solved_constrained = false;

            // Loop over all constraints and try to solve them one by one.
            let mut constraints = self.constraints.clone();
            while let Some(constraint) = constraints.pop_front() {
                match self.simplify_constraint(&constraint) {
                    SimplifyResult::Error => {
                        self.constraints.retain(|c| &**c as *const Constraint != &*constraint as *const Constraint);
                        return false;
                    },
                    SimplifyResult::Solved => {
                        self.constraints.retain(|c| &**c as *const Constraint != &*constraint as *const Constraint);
                        solved_constrained = true;
                    },
                    SimplifyResult::Unresolved => {},
                }
            }

            // If no constraint was solved, break out, we're stuck.
            if !solved_constrained {
                break;
            }
        }

        true
    }

    /// Given a constraint try to simplify it.
    fn simplify_constraint(&mut self, constraint: &Constraint) -> SimplifyResult {
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
                    (Ty::Infer(tv), _) | (_, Ty::Infer(tv)) => SimplifyResult::Unresolved,

                    // Everything else is an error
                    _ => SimplifyResult::Error
                }
            }
            ConstraintKind::NumberLiteral { ty, number_ty} => {
                let ty = self.type_variables.borrow_mut().replace_if_possible(ty);
                match (&*ty, number_ty){
                    (Ty::Float, NumberType::Float)|
                    (Ty::Float, NumberType::Integer)|
                    (Ty::Int, NumberType::Integer) => return SimplifyResult::Solved,

                    (Ty::Infer(_), _) => return SimplifyResult::Unresolved,

                     _ => SimplifyResult::Error
                }
            }
        }
    }
}