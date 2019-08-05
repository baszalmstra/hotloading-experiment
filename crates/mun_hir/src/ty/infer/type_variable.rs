use ena::unify::{UnifyValue, UnifyKey, NoError, InPlaceUnificationTable};
use std::borrow::Cow;
use crate::{Ty};
use std::fmt;

/// The ID of a type variable.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeVarId(pub(super) u32);

impl fmt::Display for TypeVarId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}", self.0)
    }
}

impl UnifyKey for TypeVarId {
    type Value = TypeVarValue;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(i: u32) -> Self {
        TypeVarId(i)
    }

    fn tag() -> &'static str {
        "TypeVarId"
    }
}

/// The value of a type variable: either we already know the type, or we don't
/// know it yet.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TypeVarValue {
    Known(Ty),
    Unknown,
}

impl TypeVarValue {
    fn known(&self) -> Option<&Ty> {
        match self {
            TypeVarValue::Known(ty) => Some(ty),
            TypeVarValue::Unknown => None,
        }
    }

    fn is_unknown(&self) -> bool {
        match self {
            TypeVarValue::Known(_) => false,
            TypeVarValue::Unknown => true
        }
    }
}

impl UnifyValue for TypeVarValue {
    type Error = NoError;

    fn unify_values(value1: &Self, value2: &Self) -> Result<Self, NoError> {
        match (value1, value2) {
            // We should never equate two type variables, both of which have
            // known types. Instead, we recursively equate those types.
            (TypeVarValue::Known(t1), TypeVarValue::Known(t2)) => panic!(
                "equating two type variables, both of which have known types: {:?} and {:?}",
                t1, t2
            ),

            // If one side is known, prefer that one.
            (TypeVarValue::Known(..), TypeVarValue::Unknown) => Ok(value1.clone()),
            (TypeVarValue::Unknown, TypeVarValue::Known(..)) => Ok(value2.clone()),

            (TypeVarValue::Unknown, TypeVarValue::Unknown) => Ok(TypeVarValue::Unknown),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct TypeVariableTable {
    eq_relations: InPlaceUnificationTable<TypeVarId>
}

impl TypeVariableTable {
    /// Creates a new generic infer type variable
    pub fn new_type_var(&mut self) -> TypeVarId {
        self.eq_relations.new_key(TypeVarValue::Unknown)
    }

    /// Records that `a == b`
    pub fn equate(&mut self, a: TypeVarId, b:TypeVarId) {
        debug_assert!(self.eq_relations.probe_value(a).is_unknown());
        debug_assert!(self.eq_relations.probe_value(b).is_unknown());
        self.eq_relations.union(a,b);
    }

    /// Instantiates `tv` with the type `ty`.
    pub fn instantiate(&mut self, tv: TypeVarId, ty: Ty) {
        debug_assert!(self.eq_relations.probe_value(tv).is_unknown(),
            "instantiating type variable `{:?}` twice: new-value = {:?}, old-value={:?}",
            tv, ty, self.eq_relations.probe_value(tv).known().unwrap());
        self.eq_relations.union_value(tv, TypeVarValue::Known(ty));
    }

    /// If `ty` is a type-inference variable, and it has been instantiated, then return the
    /// instantiated type; otherwise returns `ty`.
    pub fn replace_if_possible<'t>(&mut self, ty: &'t Ty) -> Cow<'t, Ty> {
        let mut ty = Cow::Borrowed(ty);
        match &*ty {
            Ty::Infer(tv) => {
                match self.eq_relations.probe_value(*tv).known() {
                    Some(known_ty) => Cow::Owned(known_ty.clone()),
                    _ => ty,
                }
            }
            _ => ty,
        }
    }
}