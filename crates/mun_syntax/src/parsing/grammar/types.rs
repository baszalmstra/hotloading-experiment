use super::*;

pub(super) const TYPE_RECOVERY_SET: TokenSet = token_set![R_PAREN, COMMA];

pub(super) fn ascription(p:&mut Parser) {
    p.expect(COLON);
    type_(p);
}

pub(super) fn type_(p: &mut Parser) {
    match p.current() {
        IDENT => name_ref(p),
        _ => {
            p.error_recover("expected type", TYPE_RECOVERY_SET);
        }
    }
}