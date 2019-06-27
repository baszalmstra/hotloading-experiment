pub(crate) mod src;

use std::sync::Arc;
use crate::{ids::FunctionId, FileId, Name, AsName, DefDatabase, HirDatabase};
use self::src::HasSource;
use mun_syntax::{ast::{self, NameOwner}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function {
    pub(crate) id: FunctionId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnData {
    pub(crate) name: Name,
}

impl FnData {
    pub(crate) fn fn_data_query(
        db: &(impl DefDatabase),
        func: Function,
    ) -> Arc<FnData> {
        let src = func.source(db);
        let name = src.ast.name().map(|n| n.as_name()).unwrap_or_else(Name::missing );
        Arc::new(FnData { name })
    }
}

impl Function {
    pub fn name(self, db:&impl HirDatabase) -> Name { self.data(db).name.clone() }

    pub fn data(self, db:&impl HirDatabase) -> Arc<FnData> { db.fn_data(self) }
}