use super::ConstraintSystem;
use crate::arena::map::ArenaMap;
use crate::{Ty, ExprId, PatId};
use crate::arena::Arena;

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

impl ConstraintSystem {
    /// Solve the system of constraints.
    pub fn solve(&mut self) -> SolveResult {
        let snapshot = self.snapshot();
        let result = self.solve_inner();
        self.rollback_to(snapshot);
        result
    }

    pub fn solve_inner(&mut self) -> SolveResult {
        // Start by simplifying
        if !self.simplify() {
            return SolveResult::Error;
        }

        // If there are no more constraints we found a solution
        if self.constraints.is_empty() {
            return SolveResult::Solution(self.build_solution())
        }

        // Lets try and solve each type variable
        let type_variables = self.type_variables.borrow_mut().unsolved_variables();


        SolveResult::NoSolution
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