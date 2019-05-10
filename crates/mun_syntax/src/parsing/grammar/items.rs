use super::*;

pub(super) const ITEM_RECOVERY_SET: TokenSet = token_set![
    FUNCTION_KW
];

pub(super) fn mod_contents(p: &mut Parser) {
    while !p.matches(EOF) {
        item(p);
    }
}

pub(super) fn item(p: &mut Parser) {
    let m = p.start();
    let m = match maybe_item(p, m) {
        Ok(()) => return,
        Err(m) => m
    };

    m.abandon(p);
    if p.matches(L_CURLY) {
        error_block(p, "expected an item")
    } else if p.matches(R_CURLY) {
        let e = p.start();
        p.error("unmatched }");
        p.bump();
        e.complete(p, ERROR);
    } else if !p.matches(EOF) {
        p.error_and_bump("expected an item");
    } else {
        p.error("expected an item");
    }
}

pub(super) fn maybe_item(p: &mut Parser, m: Marker) -> Result<(), Marker> {
    match p.current() {
        FUNCTION_KW => {
            fn_def(p);
            m.complete(p, FUNCTION_DEF);
        }
        _ => return Err(m)
    }
    Ok(())
}

pub(super) fn fn_def(p: &mut Parser) {
    assert!(p.matches(FUNCTION_KW));
    p.bump();

    name_recovery(p, ITEM_RECOVERY_SET);

    // TODO: Argument list
    p.expect(L_PAREN);
    p.expect(R_PAREN);

    expressions::block(p);
}