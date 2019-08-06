use std::{
    collections::{VecDeque},
    rc::Rc
};
use drop_bomb::DropBomb;
use crate::ty::infer::type_variable;
use super::{ConstraintSystem, Constraint};

pub(crate) struct Snapshot {
    constraints: VecDeque<Rc<Constraint>>,
    type_variables: type_variable::Snapshot,
    bomb: DropBomb
}

impl ConstraintSystem {
    /// Creates a snapshot of the constraint system state. This snapshot must later be committed
    /// (`commit`) or rolled back (`rollback_to()`). Nested snapshots are permitted but must be
    /// processed in a stack-like fashion.
    pub fn snapshot(&mut self) -> Snapshot {
        Snapshot {
            constraints: self.constraints.clone(),
            type_variables: self.type_variables.borrow_mut().snapshot(),
            bomb: DropBomb::new("Snapshot must be committed or rolled back"),
        }
    }

    /// Undoes all changes since the snapshot was created. Any snapshot created since that point
    /// must already have been committed or rolled back.
    pub fn rollback_to(&mut self, s: Snapshot) {
        let Snapshot { constraints, type_variables, mut bomb } = s;
        self.constraints = constraints;
        self.type_variables.borrow_mut().rollback_to(type_variables);
        bomb.defuse();
    }

    /// Commits all changes since the snapshot was created, making them permanent (unless this
    /// snapshot was created within another snapshot). Any snapshot created since that point
    /// must already have been committed or rolled back.
    pub fn commit(&mut self, s:Snapshot) {
        let Snapshot { constraints, type_variables, mut bomb } = s;
        self.type_variables.borrow_mut().commit(type_variables);
        bomb.defuse();
    }
}