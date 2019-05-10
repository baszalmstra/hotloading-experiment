use super::*;

pub(super) fn param_list(p: &mut Parser) {
    list(p)
}

fn list(p:&mut Parser) {
    assert!(p.matches(L_PAREN));
    let m = p.start();
    p.bump();
    while !p.matches(EOF) && !p.matches(R_PAREN) {
        if !p.matches(IDENT) {
            p.error("expected value parameter");
            break;
        }
        value_parameter(p);
        if !p.matches(R_PAREN) {
            p.expect(COMMA);
        }
    }
    p.expect(R_PAREN);
    m.complete(p, PARAM_LIST);
}

fn value_parameter(p: &mut Parser) {
    let m = p.start();
    name_recovery(p, token_set![COLON, R_PAREN]);
    types::ascription(p);
    m.complete(p, PARAM);
}