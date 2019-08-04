mod infer;
mod lower;
mod op;
mod type_variable;

use crate::display::{HirDisplay, HirFormatter};
use crate::{Function, HirDatabase};
pub(crate) use infer::infer_query;
pub use infer::InferenceResult;
pub(crate) use lower::{fn_sig_for_fn, type_for_def, TypableDef};
use std::{fmt, mem};
use std::sync::Arc;
use crate::ty::infer::{InferTy};
use crate::ty::type_variable::TypeVarId;

/// This should be cheap to clone.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Ty {
    Empty,

    Apply(ApplicationTy),

    /// The primitive floating point type. Written as `float`.
    Float,

    /// The primitive integral type. Written as `int`.
    Int,

    /// A type variable used during type checking. Not to be confused with a type parameter.
    Infer(TypeVarId),

    /// A placeholder for a type which could not be computed; this is propagated to avoid useless
    /// error messages. Doubles as a placeholder where type variables are inserted before type
    /// checking, since we want to try to infer a better type here anyway -- for the IDE use case,
    /// we want to try to infer as much as possible even in the presence of type errors.
    Unknown,
}

/// A nominal type with (maybe 0) type parameters. This might be a primitive
/// type like `bool`, a struct, tuple, function pointer, reference or
/// several other things.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct ApplicationTy {
    pub ctor: TypeCtor,
    pub parameters: Substs,
}

/// A type constructor or type name: this might be something like the primitive
/// type `bool`, a struct like `Vec`, or things like function pointers or
/// tuples.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum TypeCtor {
    /// The anonymous type of a function declaration/definition. Each
    /// function has a unique type, which is output (for a function
    /// named `foo` returning an `number`) as `fn() -> number {foo}`.
    ///
    /// This includes tuple struct / enum variant constructors as well.
    ///
    /// For example the type of `bar` here:
    ///
    /// ```mun
    /// function foo() -> number { 1 }
    /// let bar = foo; // bar: function() -> number {foo}
    /// ```
    FnDef(Function),
}

impl Ty {
    pub fn simple(ctor: TypeCtor) -> Ty {
        Ty::Apply(ApplicationTy {
            ctor,
            parameters: Substs::empty(),
        })
    }

    pub fn as_simple(&self) -> Option<TypeCtor> {
        match self {
            Ty::Apply(ApplicationTy { ctor, parameters}) if parameters.0.is_empty() => Some(*ctor),
            _ => None
        }
    }

    fn walk_mut(&mut self, f: &mut impl FnMut(&mut Ty)) {
        match self {
            Ty::Apply(a_ty) => {
                a_ty.parameters.walk_mut(f);
            }
            Ty::Empty
            | Ty::Float
            | Ty::Int
            | Ty::Infer(_)
            | Ty::Unknown => {}
        }
        f(self);
    }

    fn fold(mut self, f: &mut impl FnMut(Ty) -> Ty) -> Ty {
        self.walk_mut(&mut |ty_mut| {
            let ty = mem::replace(ty_mut, Ty::Unknown);
            *ty_mut = f(ty);
        });
        self
    }
}

/// A list of substitutions for generic parameters.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Substs(Arc<[Ty]>);

impl Substs {
    pub fn empty() -> Substs {
        Substs(Arc::new([]))
    }

    pub fn single(ty: Ty) -> Substs {
        Substs(Arc::new([ty]))
    }

    pub fn walk_mut(&mut self, f: &mut impl FnMut(&mut Ty)) {
        // Without an Arc::make_mut_slice, we can't avoid the clone here:
        let mut v: Vec<_> = self.0.iter().cloned().collect();
        for t in &mut v {
            t.walk_mut(f);
        }
        self.0 = v.into();
    }
}

/// A function signature as seen by type inference: Several parameter types and
/// one return type.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FnSig {
    params_and_return: Arc<[Ty]>,
}

impl FnSig {
    pub fn from_params_and_return(mut params: Vec<Ty>, ret: Ty) -> FnSig {
        params.push(ret);
        FnSig {
            params_and_return: params.into(),
        }
    }

    pub fn params(&self) -> &[Ty] {
        &self.params_and_return[0..self.params_and_return.len() - 1]
    }

    pub fn ret(&self) -> &Ty {
        &self.params_and_return[self.params_and_return.len() - 1]
    }
}

impl HirDisplay for Ty {
    fn hir_fmt(&self, f: &mut HirFormatter<impl HirDatabase>) -> fmt::Result {
        match self {
            Ty::Apply(a_ty) => a_ty.hir_fmt(f)?,
            Ty::Float => write!(f, "float")?,
            Ty::Int => write!(f, "int")?,
            Ty::Unknown => write!(f, "{{unknown}}")?,
            Ty::Empty => write!(f, "nothing")?,
            Ty::Infer(tv) => write!(f, "'{}", tv.0)?,
        }
        Ok(())
    }
}

impl HirDisplay for ApplicationTy {
    fn hir_fmt(&self, f: &mut HirFormatter<impl HirDatabase>) -> fmt::Result {
        match self.ctor {
            TypeCtor::FnDef(def) => {
                let sig = f.db.fn_signature(def);
                let name = def.name(f.db);
                write!(f, "function {}", name)?;
                write!(f, "(")?;
                f.write_joined(sig.params(), ", ")?;
                write!(f, ") -> {}", sig.ret().display(f.db))?;
            }
            _ => write!(f, "fn")?,
        }
        Ok(())
    }
}

impl HirDisplay for &Ty {
    fn hir_fmt(&self, f: &mut HirFormatter<impl HirDatabase>) -> fmt::Result {
        HirDisplay::hir_fmt(*self, f)
    }
}
