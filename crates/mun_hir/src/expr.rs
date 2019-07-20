use crate::{
    arena::map::ArenaMap,
    arena::{Arena, RawId},
    code_model::DefWithBody,
    FileId, HirDatabase, Name, Path,
};

//pub use mun_syntax::ast::PrefixOp as UnaryOp;
use crate::source_id::AstId;
use crate::code_model::src::HasSource;
use crate::name::AsName;
use crate::type_ref::TypeRef;
pub use mun_syntax::ast::BinOp as BinaryOp;
pub use mun_syntax::ast::PrefixOp as UnaryOp;
use mun_syntax::ast::{ArgListOwner, NameOwner, TypeAscriptionOwner};
use mun_syntax::{ast, AstNode, AstPtr, SyntaxNodePtr};
use rustc_hash::FxHashMap;
use std::ops::Index;
use std::sync::Arc;

pub use self::scope::ExprScopes;
use crate::resolve::Resolver;

pub(crate) mod scope;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId(RawId);
impl_arena_id!(ExprId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PatId(RawId);
impl_arena_id!(PatId);

/// The body of an item (function, const etc.).
#[derive(Debug, Eq, PartialEq)]
pub struct Body {
    owner: DefWithBody,
    exprs: Arena<ExprId, Expr>,
    pats: Arena<PatId, Pat>,
    /// The patterns for the function's parameters. While the parameter types are part of the
    /// function signature, the patterns are not (they don't change the external type of the
    /// function).
    ///
    /// If this `Body` is for the body of a constant, this will just be empty.
    params: Vec<PatId>,
    /// The `ExprId` of the actual body expression.
    body_expr: ExprId,
}

impl Body {
    pub fn params(&self) -> &[PatId] {
        &self.params
    }

    pub fn body_expr(&self) -> ExprId {
        self.body_expr
    }

    pub fn owner(&self) -> DefWithBody {
        self.owner
    }

    pub fn exprs(&self) -> impl Iterator<Item = (ExprId, &Expr)> {
        self.exprs.iter()
    }

    pub fn pats(&self) -> impl Iterator<Item = (PatId, &Pat)> {
        self.pats.iter()
    }
}

impl Index<ExprId> for Body {
    type Output = Expr;

    fn index(&self, expr: ExprId) -> &Expr {
        &self.exprs[expr]
    }
}

impl Index<PatId> for Body {
    type Output = Pat;

    fn index(&self, pat: PatId) -> &Pat {
        &self.pats[pat]
    }
}

/// An item body together with the mapping from syntax nodes to HIR expression Ids. This is needed
/// to go from e.g. a position in a file to the HIR expression containing it; but for type
/// inference etc., we want to operate on a structure that is agnostic to the action positions of
/// expressions in the file, so that we don't recompute types whenever some whitespace is typed.
#[derive(Default, Debug, Eq, PartialEq)]
pub struct BodySourceMap {
    expr_map: FxHashMap<SyntaxNodePtr, ExprId>,
    expr_map_back: ArenaMap<ExprId, SyntaxNodePtr>,
    pat_map: FxHashMap<AstPtr<ast::Pat>, PatId>,
    pat_map_back: ArenaMap<PatId, AstPtr<ast::Pat>>,
}

impl BodySourceMap {
    pub(crate) fn expr_syntax(&self, expr: ExprId) -> Option<SyntaxNodePtr> {
        self.expr_map_back.get(expr).cloned()
    }

    pub(crate) fn syntax_expr(&self, ptr: SyntaxNodePtr) -> Option<ExprId> {
        self.expr_map.get(&ptr).cloned()
    }

    pub(crate) fn node_expr(&self, node: &ast::Expr) -> Option<ExprId> {
        self.expr_map
            .get(&SyntaxNodePtr::new(node.syntax()))
            .cloned()
    }

    pub(crate) fn pat_syntax(&self, pat: PatId) -> Option<AstPtr<ast::Pat>> {
        self.pat_map_back.get(pat).cloned()
    }

    pub(crate) fn node_pat(&self, node: &ast::Pat) -> Option<PatId> {
        self.pat_map.get(&AstPtr::new(node)).cloned()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    Let {
        pat: PatId,
        type_ref: Option<TypeRef>,
        initializer: Option<ExprId>,
    },
    Expr(ExprId),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    String(String),
    Bool(bool),
    Int(u64),
    Float(u64),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    /// Used if the syntax tree does not have a required expression piece
    Missing,
    Call {
        callee: ExprId,
        args: Vec<ExprId>,
    },
    Path(Path),
    UnaryOp {
        expr: ExprId,
        op: UnaryOp,
    },
    BinaryOp {
        lhs: ExprId,
        rhs: ExprId,
        op: Option<BinaryOp>,
    },
    Block {
        statements: Vec<Statement>,
        tail: Option<ExprId>,
    },
    Literal(Literal),
}

impl Expr {
    pub fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        match self {
            Expr::Missing => {}
            Expr::Path(_) => {}
            Expr::Block { statements, tail } => {
                for stmt in statements {
                    match stmt {
                        Statement::Let { initializer, .. } => {
                            if let Some(expr) = initializer {
                                f(*expr);
                            }
                        }
                        Statement::Expr(e) => f(*e),
                    }
                }
                if let Some(expr) = tail {
                    f(*expr);
                }
            }
            Expr::Call { callee, args } => {
                f(*callee);
                for arg in args {
                    f(*arg);
                }
            }
            Expr::BinaryOp { lhs, rhs, .. } => {
                f(*lhs);
                f(*rhs);
            }
            Expr::UnaryOp { expr, .. } => {
                f(*expr);
            }
            Expr::Literal(_) => {}
        }
    }
}

// Similar to ast::PatKind
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pat {
    Missing,
    Wild,
    Path(Path),
    Bind { name: Name },
}

impl Pat {
    pub fn walk_child_pats(&self, mut f: impl FnMut(PatId)) {}
}

// Queries

pub(crate) struct ExprCollector<DB> {
    db: DB,
    owner: DefWithBody,
    exprs: Arena<ExprId, Expr>,
    pats: Arena<PatId, Pat>,
    source_map: BodySourceMap,
    params: Vec<PatId>,
    body_expr: Option<ExprId>,
}

impl<'a, DB> ExprCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn new(owner: DefWithBody, file_id: FileId, db: &'a DB) -> Self {
        ExprCollector {
            owner,
            db,
            exprs: Arena::default(),
            pats: Arena::default(),
            source_map: BodySourceMap::default(),
            params: Vec::new(),
            body_expr: None,
        }
    }

    fn alloc_pat(&mut self, pat: Pat, ptr: AstPtr<ast::Pat>) -> PatId {
        let id = self.pats.alloc(pat);
        self.source_map.pat_map.insert(ptr, id);
        self.source_map.pat_map_back.insert(id, ptr);
        id
    }

    fn alloc_expr(&mut self, expr: Expr, syntax_ptr: SyntaxNodePtr) -> ExprId {
        let id = self.exprs.alloc(expr);
        self.source_map.expr_map.insert(syntax_ptr, id);
        self.source_map.expr_map_back.insert(id, syntax_ptr);
        id
    }

    fn collect_fn_body(&mut self, node: &ast::FunctionDef) {
        if let Some(param_list) = node.param_list() {
            for param in param_list.params() {
                let pat = if let Some(pat) = param.pat() {
                    pat
                } else {
                    continue;
                };
                let param_pat = self.collect_pat(pat);
                self.params.push(param_pat);
            }
        }

        let body = self.collect_block_opt(node.body());
        self.body_expr = Some(body);
    }

    fn collect_block_opt(&mut self, block: Option<ast::Block>) -> ExprId {
        if let Some(block) = block {
            self.collect_block(block)
        } else {
            self.exprs.alloc(Expr::Missing)
        }
    }

    fn collect_block(&mut self, block: ast::Block) -> ExprId {
        let statements = block
            .statements()
            .map(|s| match s.kind() {
                ast::StmtKind::LetStmt(stmt) => {
                    let pat = self.collect_pat_opt(stmt.pat());
                    let type_ref = stmt.ascribed_type().map(TypeRef::from_ast);
                    let initializer = stmt.initializer().map(|e| self.collect_expr(e));
                    Statement::Let {
                        pat,
                        type_ref,
                        initializer,
                    }
                }
                ast::StmtKind::ExprStmt(stmt) => {
                    Statement::Expr(self.collect_expr_opt(stmt.expr()))
                }
            })
            .collect();
        let tail = block.expr().map(|e| self.collect_expr(e));
        self.alloc_expr(
            Expr::Block { statements, tail },
            SyntaxNodePtr::new(block.syntax()),
        )
    }

    fn collect_pat_opt(&mut self, pat: Option<ast::Pat>) -> PatId {
        if let Some(pat) = pat {
            self.collect_pat(pat)
        } else {
            self.pats.alloc(Pat::Missing)
        }
    }

    fn collect_expr_opt(&mut self, expr: Option<ast::Expr>) -> ExprId {
        if let Some(expr) = expr {
            self.collect_expr(expr)
        } else {
            self.exprs.alloc(Expr::Missing)
        }
    }

    fn collect_expr(&mut self, expr: ast::Expr) -> ExprId {
        let syntax_ptr = SyntaxNodePtr::new(expr.syntax());
        match expr.kind() {
            ast::ExprKind::Literal(e) => {
                let lit = match e.kind() {
                    ast::LiteralKind::Bool => Literal::Bool(Default::default()),
                    ast::LiteralKind::IntNumber => Literal::Int(Default::default()),
                    ast::LiteralKind::FloatNumber => Literal::Float(Default::default()),
                    ast::LiteralKind::String => Literal::String(Default::default()),
                };
                self.alloc_expr(Expr::Literal(lit), syntax_ptr)
            }
            ast::ExprKind::PrefixExpr(e) => {
                let expr = self.collect_expr_opt(e.expr());
                if let Some(op) = e.op_kind() {
                    self.alloc_expr(Expr::UnaryOp { expr, op }, syntax_ptr)
                } else {
                    self.alloc_expr(Expr::Missing, syntax_ptr)
                }
            }
            ast::ExprKind::BinExpr(e) => {
                let lhs = self.collect_expr_opt(e.lhs());
                let rhs = self.collect_expr_opt(e.rhs());
                let op = e.op_kind();
                self.alloc_expr(Expr::BinaryOp { lhs, rhs, op }, syntax_ptr)
            }
            ast::ExprKind::PathExpr(e) => {
                let path = e
                    .path()
                    .and_then(Path::from_ast)
                    .map(Expr::Path)
                    .unwrap_or(Expr::Missing);
                self.alloc_expr(path, syntax_ptr)
            }
            ast::ExprKind::ParenExpr(e) => {
                let inner = self.collect_expr_opt(e.expr());
                // make the paren expr point to the inner expression as well
                self.source_map.expr_map.insert(syntax_ptr, inner);
                inner
            }
            ast::ExprKind::CallExpr(e) => {
                let callee = self.collect_expr_opt(e.expr());
                let args = if let Some(arg_list) = e.arg_list() {
                    arg_list.args().map(|e| self.collect_expr(e)).collect()
                } else {
                    Vec::new()
                };
                self.alloc_expr(Expr::Call { callee, args }, syntax_ptr)
            }
            _ => unreachable!(),
        }
    }

    fn collect_pat(&mut self, pat: ast::Pat) -> PatId {
        let pattern = match pat.kind() {
            ast::PatKind::BindPat(bp) => {
                let name = bp
                    .name()
                    .map(|nr| nr.as_name())
                    .unwrap_or_else(Name::missing);
                Pat::Bind { name }
            }
            ast::PatKind::PlaceholderPat(_) => Pat::Wild,
        };
        let ptr = AstPtr::new(&pat);
        self.alloc_pat(pattern, ptr)
    }

    fn finish(self) -> (Body, BodySourceMap) {
        let body = Body {
            owner: self.owner,
            exprs: self.exprs,
            pats: self.pats,
            params: self.params,
            body_expr: self.body_expr.expect("A body should have been collected"),
        };
        (body, self.source_map)
    }
}

pub(crate) fn body_with_source_map_query(
    db: &impl HirDatabase,
    def: DefWithBody,
) -> (Arc<Body>, Arc<BodySourceMap>) {
    let mut collector;

    match def {
        DefWithBody::Function(ref f) => {
            let src = f.source(db);
            collector = ExprCollector::new(def, src.file_id, db);
            collector.collect_fn_body(&src.ast)
        }
    }

    let (body, source_map) = collector.finish();
    (Arc::new(body), Arc::new(source_map))
}

pub(crate) fn body_hir_query(db: &impl HirDatabase, def: DefWithBody) -> Arc<Body> {
    db.body_with_source_map(def).0
}

// needs arbitrary_self_types to be a method... or maybe move to the def?
pub fn resolver_for_expr(body: Arc<Body>, db: &impl HirDatabase, expr_id: ExprId) -> Resolver {
    let scopes = db.expr_scopes(body.owner);
    resolver_for_scope(body, db, scopes.scope_for(expr_id))
}

pub(crate) fn resolver_for_scope(
    body: Arc<Body>,
    db: &impl HirDatabase,
    scope_id: Option<scope::ScopeId>,
) -> Resolver {
    let mut r = body.owner.resolver(db);
    let scopes = db.expr_scopes(body.owner);
    let scope_chain = scopes.scope_chain(scope_id).collect::<Vec<_>>();
    for scope in scope_chain.into_iter().rev() {
        r = r.push_expr_scope(Arc::clone(&scopes), scope);
    }
    r
}
