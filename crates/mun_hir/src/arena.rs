use std::fmt;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

pub mod map;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RawId(u32);

impl From<RawId> for u32 {
    fn from(raw: RawId) -> u32 {
        raw.0
    }
}

impl From<u32> for RawId {
    fn from(id: u32) -> RawId {
        RawId(id)
    }
}

impl fmt::Debug for RawId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for RawId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Enables storing in an `Arena`.
pub trait ArenaId {
    fn from_raw(raw: RawId) -> Self;
    fn into_raw(self) -> RawId;
}

#[derive(Clone, PartialEq, Eq)]
pub struct Arena<ID: ArenaId, T> {
    data: Vec<T>,
    _ty: PhantomData<ID>,
}

impl<ID: ArenaId, T: fmt::Debug> fmt::Debug for Arena<ID, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Arena")
            .field("len", &self.len())
            .field("data", &self.data)
            .finish()
    }
}

impl<ID: ArenaId, T> Arena<ID, T> {
    /// Returns the number of elements in the arena
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns `true` if the arena does not contain any element
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Stores `value` in the arena and returns the associated Id.
    pub fn alloc(&mut self, value: T) -> ID {
        let id = RawId(self.data.len() as u32);
        self.data.push(value);
        ID::from_raw(id)
    }

    /// Iterate over the elements in the arena
    pub fn iter(&self) -> impl Iterator<Item = (ID, &T)> {
        self.data
            .iter()
            .enumerate()
            .map(|(idx, value)| (ID::from_raw(RawId(idx as u32)), value))
    }
}

impl<ID: ArenaId, T> Default for Arena<ID, T> {
    fn default() -> Arena<ID, T> {
        Arena {
            data: Vec::new(),
            _ty: PhantomData,
        }
    }
}

impl<ID: ArenaId, T> Index<ID> for Arena<ID, T> {
    type Output = T;
    fn index(&self, idx: ID) -> &T {
        let idx = idx.into_raw().0 as usize;
        &self.data[idx]
    }
}

impl<ID: ArenaId, T> IndexMut<ID> for Arena<ID, T> {
    fn index_mut(&mut self, idx: ID) -> &mut T {
        let idx = idx.into_raw().0 as usize;
        &mut self.data[idx]
    }
}

impl<ID: ArenaId, T> FromIterator<T> for Arena<ID, T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        Arena {
            data: Vec::from_iter(iter),
            _ty: PhantomData,
        }
    }
}

/// Implements `ArenaId` for the specified type.
macro_rules! impl_arena_id {
    ($name:ident) => {
        impl $crate::ArenaId for $name {
            fn from_raw(raw: $crate::RawId) -> Self {
                $name(raw)
            }
            fn into_raw(self) -> $crate::RawId {
                self.0
            }
        }
    };
}
