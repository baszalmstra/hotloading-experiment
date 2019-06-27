use mun_syntax::ast::{self, FunctionDef, ModuleItemOwner, NameOwner};

use crate::{Arena, DefDatabase, FileAstId, FileId, RawId, Name};
use std::ops::Index;
use std::sync::Arc;
use crate::name::AsName;
use core::borrow::BorrowMut;

/// Id for a module definition
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct DefId(RawId);
impl_arena_id!(DefId);

#[derive(Debug, PartialEq, Eq)]
pub(super) struct DefData {
    pub(super) name: Name,
    pub(super) kind: DefKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(super) enum DefKind {
    Function(FileAstId<ast::FunctionDef>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(super) enum RawFileItem {
    Definition(DefId),
}

/// `RawFileItems` are top level file items. `RawFileItems` do not change on most edits.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct RawFileItems {
    definitions: Arena<DefId, DefData>,
    items: Vec<RawFileItem>,
}

impl Index<DefId> for RawFileItems {
    type Output = DefData;

    fn index(&self, index: DefId) -> &Self::Output {
        &self.definitions[index]
    }
}

impl RawFileItems {
    pub(crate) fn raw_file_items_query(db: &impl DefDatabase, file_id: FileId) -> Arc<RawFileItems> {
        let mut items = RawFileItems::default();
        let source_file = db.parse(file_id);
        let ast_id_map = db.ast_id_map(file_id);
        for item in source_file.items() {
            let (kind, name) = match item.kind() {
                ast::ModuleItemKind::FunctionDef(it) => {
                    (DefKind::Function((*ast_id_map).ast_id(it)), it.name())
                }
            };

            // If no name is provided an error is already emitted
            if let Some(name) = name {
                let id = items.definitions.alloc(DefData { name: name.as_name(), kind });
                items.items.push(RawFileItem::Definition(id));
            }
        }
        Arc::new(items)
    }

    pub(super) fn items(&self) -> &[RawFileItem] {
        &self.items
    }
}
