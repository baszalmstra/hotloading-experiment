use crate::{Context, IRCodeGen};
use inkwell::basic_block::BasicBlock;
use inkwell::types::{BasicTypeEnum, FloatType};
use inkwell::values::*;
use mun_syntax::ast::StmtKind::ExprStmt;
use mun_syntax::ast::{AstNode, NameOwner, StmtKind, TypeAscriptionOwner};
use mun_syntax::{ast, SyntaxKind};
use std::collections::HashMap;

pub(crate) fn emit(fun: &ast::FunctionDef, context: &mut Context) {
    let name = fun.name().expect("Missing name");

    let arguments: Vec<BasicTypeEnum> = fun
        .param_list()
        .expect("missing param list")
        .params()
        .map(|arg| {
            let ty = arg.ascribed_type().expect("missing argument type");
            super::types::known_type(ty, context)
                .expect("unknown type")
                .into()
        })
        .collect();

    let fn_type = match fun.ret_type().and_then(|r| r.type_ref()) {
        Some(t) => super::types::known_type(t, context)
            .expect("Unknown type")
            .fn_type(arguments.as_slice(), false),
        None => inkwell::types::VoidType::void_type().fn_type(arguments.as_slice(), false),
    };

    let name = name.syntax().text().to_string();
    let fn_ref = context.module.add_function(&name, fn_type, None);

    let body = context
        .module
        .get_context()
        .append_basic_block(&fn_ref, "body");
    context.builder.position_at_end(&body);

    let mut symbol_table: HashMap<String, BasicValueEnum> = HashMap::new();
    for (idx, arg) in fun
        .param_list()
        .expect("missing param list")
        .params()
        .enumerate()
    {
        let name = arg
            .pat()
            .expect("missing arg name")
            .syntax()
            .text()
            .to_string();
        let value = fn_ref.get_nth_param(idx as u32).expect("missing func arg");
        symbol_table.insert(name, value);
    }

    for statement in fun.body().expect("missing body").statements() {
        match statement.kind() {
            StmtKind::ExprStmt(statement) => {
                emit_expr_stmt(statement, context, &mut symbol_table);
            }
            _ => unreachable!(),
        }
    }

    if fun.ret_type().is_some() {
        let ret_expr = emit_expr(
            fun.body()
                .expect("missing body")
                .expr()
                .expect("missing return expression"),
            context,
            &mut symbol_table,
        );

        context
            .builder
            .build_return(Some(&ret_expr.expect("no return value")));
    }

    context.functions.insert(name, fn_ref);
}

pub(crate) fn emit_expr_stmt(
    stmt: &ast::ExprStmt,
    context: &mut Context,
    symbols: &mut HashMap<String, BasicValueEnum>,
) -> Option<BasicValueEnum> {
    emit_expr(stmt.expr()?, context, symbols)
}

pub(crate) fn emit_expr(
    expr: &ast::Expr,
    context: &mut Context,
    symbols: &mut HashMap<String, BasicValueEnum>,
) -> Option<BasicValueEnum> {
    match expr.kind() {
        ast::ExprKind::PathExpr(name) => symbols.get(&name.syntax().text().to_string()).map(|v| *v),
        ast::ExprKind::Literal(lit) => match lit.syntax().first_token().unwrap().kind() {
            SyntaxKind::FLOAT_NUMBER | SyntaxKind::INT_NUMBER => {
                let value = lit.syntax().text().to_string().parse::<f64>().ok()?;
                Some(
                    context
                        .module
                        .get_context()
                        .f32_type()
                        .const_float(value)
                        .into(),
                )
            }
            _ => unreachable!(),
        },
        ast::ExprKind::BinExpr(expr) => {
            let lhs = emit_expr(expr.lhs().expect("missing lhs"), context, symbols)?;
            let rhs = emit_expr(expr.rhs().expect("missing rhs"), context, symbols)?;
            match expr.op_kind().expect("missing op") {
                ast::BinOp::Add => {
                    Some(BasicValueEnum::FloatValue(context.builder.build_float_add(
                        *lhs.as_float_value(),
                        *rhs.as_float_value(),
                        "binadd",
                    )))
                }
                _ => unreachable!(),
            }
        }
        _ => None,
        //        ExprKind::PrefixExpr(_) => {},
        //        ExprKind::ParenExpr(_) => {},
        //        ExprKind::CallExpr(_) => {},
    }
}
