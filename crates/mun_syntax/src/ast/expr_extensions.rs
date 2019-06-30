use super::{children, BinExpr};
use crate::{ast, AstNode, SmolStr, SyntaxKind, SyntaxToken};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Subtract,
    Divide,
    Multiply,
    Remainder,
    Power,
    Assign,
    AddAssign,
    SubtractAssign,
    DivideAssign,
    MultiplyAssign,
    RemainderAssign,
    PowerAssign,
}

impl BinExpr {
    pub fn op_details(&self) -> Option<(SyntaxToken, BinOp)> {
        use SyntaxKind::*;
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.as_token())
            .find_map(|c| match c.kind() {
                PLUS => Some((c, BinOp::Add)),
                MINUS => Some((c, BinOp::Subtract)),
                SLASH => Some((c, BinOp::Divide)),
                STAR => Some((c, BinOp::Multiply)),
                PERCENT => Some((c, BinOp::Remainder)),
                CARET => Some((c, BinOp::Power)),
                EQ => Some((c, BinOp::Assign)),
                PLUSEQ => Some((c, BinOp::AddAssign)),
                MINUSEQ => Some((c, BinOp::SubtractAssign)),
                SLASHEQ => Some((c, BinOp::DivideAssign)),
                STAREQ => Some((c, BinOp::MultiplyAssign)),
                PERCENTEQ => Some((c, BinOp::RemainderAssign)),
                CARETEQ => Some((c, BinOp::PowerAssign)),
                _ => None,
            })
    }

    pub fn op_kind(&self) -> Option<BinOp> {
        self.op_details().map(|t| t.1)
    }

    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.op_details().map(|t| t.0)
    }

    pub fn lhs(&self) -> Option<&ast::Expr> {
        children(self).nth(0)
    }

    pub fn rhs(&self) -> Option<&ast::Expr> {
        children(self).nth(1)
    }

    pub fn sub_exprs(&self) -> (Option<&ast::Expr>, Option<&ast::Expr>) {
        let mut children = children(self);
        let first = children.next();
        let second = children.next();
        (first, second)
    }
}
