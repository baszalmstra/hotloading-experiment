mod infer;
mod lower;

use crate::display::{HirDisplay, HirFormatter};
use crate::{Function, HirDatabase};
pub(crate) use infer::{infer_query, InferenceResult};
pub(crate) use lower::{type_for_def, fn_sig_for_fn, TypableDef};
use std::fmt;
use std::sync::Arc;

/// This should be cheap to clone.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Ty {
    Empty,

    Apply(ApplicationTy),

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
    /// The primitive number type. Written as `number`.
    Number,

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
            Ty::Unknown => write!(f, "{{unknown}}")?,
            Ty::Empty => write!(f, "empty")?,
        }
        Ok(())
    }
}

impl HirDisplay for ApplicationTy {
    fn hir_fmt(&self, f: &mut HirFormatter<impl HirDatabase>) -> fmt::Result {
        match self.ctor {
            TypeCtor::Number => write!(f, "number")?,
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
