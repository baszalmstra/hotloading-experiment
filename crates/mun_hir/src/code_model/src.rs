use crate::code_model::Function;
use crate::ids::AstItemDef;
use crate::{DefDatabase, FileId};
use mun_syntax::{ast, TreeArc};

pub struct Source<T> {
    pub file_id: FileId,
    pub ast: T,
}

pub trait HasSource {
    type Ast;
    fn source(self, db: &impl DefDatabase) -> Source<Self::Ast>;
}

impl HasSource for Function {
    type Ast = TreeArc<ast::FunctionDef>;
    fn source(self, db: &impl DefDatabase) -> Source<Self::Ast> {
        self.id.source(db)
    }
}
