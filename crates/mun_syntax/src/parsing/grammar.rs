mod declarations;
mod expressions;
mod params;
mod types;
mod paths;

use super::{
    parser::{CompletedMarker, Marker, Parser},
    token_set::TokenSet,
    SyntaxKind::{self, *},
};

pub(crate) fn root(p: &mut Parser) {
    let m = p.start();
    declarations::mod_contents(p);
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

fn name_ref(p: &mut Parser) {
    if p.matches(IDENT) {
        let m = p.start();
        p.bump();
        m.complete(p, NAME_REF);
    } else {
        p.error_and_bump("expected identifier");
    }
}

fn opt_visibility(p: &mut Parser) -> bool {
    if p.matches(EXPORT_KW) {
        let m = p.start();
        p.bump();
        m.complete(p, VISIBILITY);
        true
    } else {
        false
    }
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

pub(crate) fn path(p: &mut Parser) {
    paths::type_path(p);
}