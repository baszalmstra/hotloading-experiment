use super::*;

pub(crate) fn expr_block_contents(p: &mut Parser) {
    while !p.matches(EOF) && !p.matches(R_CURLY) {
        p.bump();
        continue;
    }
}

pub(crate) fn block(p: &mut Parser) {
    if !p.matches(L_CURLY) {
        p.error("expected a block");
        return;
    }
    let m = p.start();
    expr_block_contents(p);
    p.expect(R_CURLY);
    m.complete(p, BLOCK);
}