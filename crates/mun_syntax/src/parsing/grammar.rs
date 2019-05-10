use super::{
    parser::Parser,
    SyntaxKind::{self, *}
};


pub(crate) fn root(p: &mut Parser) {
    let m = p.start();
    m.complete(p, SOURCE_FILE);
}