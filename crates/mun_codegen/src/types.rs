use super::*;
use inkwell;
use mun_syntax::ast::AstNode;

pub(crate) fn known_type(ty: &ast::TypeRef, ctx: &Context) -> Option<inkwell::types::FloatType> {
    match ty.syntax().text().to_string().as_str() {
        "number" => Some(inkwell::types::FloatType::f32_type()),
        _ => None,
    }
}
