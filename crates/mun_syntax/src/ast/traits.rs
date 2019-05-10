use crate::ast::{self, AstChildren, AstNode, children};

pub trait ModuleItemOwner: AstNode {
    fn items(&self) -> AstChildren<ast::ModuleItem> {
        children(self)
    }
}

pub trait FunctionDefOwner: AstNode {
    fn functions(&self) -> AstChildren<ast::FunctionDef> {
        children(self)
    }
}