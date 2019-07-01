pub(crate) mod src;

use self::src::HasSource;
use crate::ids::AstItemDef;
use crate::ids::LocationCtx;
use crate::raw::{DefKind, RawFileItem};
use crate::type_ref::TypeRef;
use crate::{ids::FunctionId, AsName, DefDatabase, FileId, HirDatabase, Name};
use mun_syntax::ast::{self, NameOwner, TypeAscriptionOwner};
use std::sync::Arc;
use crate::expr::{Body, BodySourceMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Module {
    pub(crate) file_id: FileId,
}

impl Module {
    pub fn package_modules(db: &impl DefDatabase) -> Vec<Module> {
        db.package_input()
            .modules()
            .map(|m| Module { file_id: m })
            .collect()
    }

    /// Returns all the definitions declared in this module.
    pub fn declarations(self, db: &impl HirDatabase) -> Vec<ModuleDef> {
        db.module_data(self.file_id).definitions.clone()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
pub struct ModuleData {
    definitions: Vec<ModuleDef>,
}

impl ModuleData {
    pub(crate) fn module_data_query(db: &impl DefDatabase, file_id: FileId) -> Arc<ModuleData> {
        let items = db.raw_items(file_id);
        let mut data = ModuleData::default();
        let loc_ctx = LocationCtx::new(db, file_id);
        for item in items.items().iter() {
            match item {
                RawFileItem::Definition(def) => match items[*def].kind {
                    DefKind::Function(ast_id) => {
                        data.definitions.push(ModuleDef::Function(Function {
                            id: FunctionId::from_ast_id(loc_ctx, ast_id),
                        }))
                    }
                },
            };
        }
        Arc::new(data)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleDef {
    Function(Function),
}

/// The definitions that have a body.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefWithBody {
    Function(Function),
}

impl From<Function> for DefWithBody {
    fn from(f: Function) -> Self {
        DefWithBody::Function(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function {
    pub(crate) id: FunctionId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnData {
    pub(crate) name: Name,
    pub(crate) params: Vec<TypeRef>,
    pub(crate) ret_type: TypeRef,
}

impl FnData {
    pub(crate) fn fn_data_query(db: &(impl DefDatabase), func: Function) -> Arc<FnData> {
        let src = func.source(db);
        let name = src
            .ast
            .name()
            .map(|n| n.as_name())
            .unwrap_or_else(Name::missing);

        let mut params = Vec::new();
        if let Some(param_list) = src.ast.param_list() {
            for param in param_list.params() {
                let type_ref = TypeRef::from_ast_opt(param.ascribed_type());
                params.push(type_ref);
            }
        }

        let ret_type = if let Some(type_ref) = src.ast.ret_type().and_then(|rt| rt.type_ref()) {
            TypeRef::from_ast(type_ref)
        } else {
            TypeRef::Error
        };

        Arc::new(FnData {
            name,
            params,
            ret_type,
        })
    }

    pub fn name(&self) -> &Name { &self.name }

    pub fn params(&self) -> &[TypeRef] { &self.params }

    pub fn ret_type(&self) -> &TypeRef { &self.ret_type }
}

impl Function {
    pub fn module(self, db: &impl DefDatabase) -> FileId {
        self.id.file_id(db)
    }

    pub fn name(self, db: &impl HirDatabase) -> Name {
        self.data(db).name.clone()
    }

    pub fn data(self, db: &impl HirDatabase) -> Arc<FnData> {
        db.fn_data(self)
    }

    pub fn body(self, db: &impl HirDatabase) -> Arc<Body> {
        db.body_hir(self.into())
    }

    pub(crate) fn body_source_map(self, db: &impl HirDatabase) -> Arc<BodySourceMap> {
        db.body_with_source_map(self.into()).1
    }
}