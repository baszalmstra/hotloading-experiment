pub(crate) mod src;

use self::src::HasSource;
use crate::ids::AstItemDef;
use crate::ids::LocationCtx;
use crate::raw::{DefKind, RawFileItem};
use crate::{ids::FunctionId, AsName, DefDatabase, FileId, HirDatabase, Name};
use mun_syntax::ast::{self, NameOwner};
use std::sync::Arc;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function {
    pub(crate) id: FunctionId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnData {
    pub(crate) name: Name,
}

impl FnData {
    pub(crate) fn fn_data_query(db: &(impl DefDatabase), func: Function) -> Arc<FnData> {
        let src = func.source(db);
        let name = src
            .ast
            .name()
            .map(|n| n.as_name())
            .unwrap_or_else(Name::missing);
        Arc::new(FnData { name })
    }
}

impl Function {
    pub fn name(self, db: &impl HirDatabase) -> Name {
        self.data(db).name.clone()
    }

    pub fn data(self, db: &impl HirDatabase) -> Arc<FnData> {
        db.fn_data(self)
    }
}

/// Compare ty::Ty
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeRef {

}