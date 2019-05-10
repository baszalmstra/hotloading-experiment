mod expressions;
mod items;

use super::{
    parser::{Marker, CompletedMarker, Parser},
    SyntaxKind::{self, *},
    token_set::TokenSet
};

pub(crate) fn root(p: &mut Parser) {
    let m = p.start();
    items::mod_contents(p);
    m.complete(p, SOURCE_FILE);
}

fn name_recovery(p: &mut Parser, recovery: TokenSet) {
    if p.matches(IDENT) {
        let m = p.start();
        p.bump();
        m.complete(p, NAME);
    } else {
        p.error_recover("expected a name", recovery)
    }
}

fn name(p: &mut Parser) {
    name_recovery(p, TokenSet::empty())
}

fn error_block(p: &mut Parser, message: &str) {
    assert!(p.matches(L_CURLY));
    let m = p.start();
    p.error(message);
    p.bump();
    expressions::expr_block_contents(p);
    p.eat(R_CURLY);
    m.complete(p, ERROR);
}
