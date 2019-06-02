use super::*;

pub(super) const DECLARATION_RECOVERY_SET: TokenSet = token_set![FUNCTION_KW, EXPORT_KW];

pub(super) fn mod_contents(p: &mut Parser) {
    while !p.matches(EOF) {
        declaration(p);
    }
}

pub(super) fn declaration(p: &mut Parser) {
    let m = p.start();
    let m = match maybe_declaration(p, m) {
        Ok(()) => return,
        Err(m) => m,
    };

    m.abandon(p);
    if p.matches(L_CURLY) {
        error_block(p, "expected a declaration")
    } else if p.matches(R_CURLY) {
        let e = p.start();
        p.error("unmatched }");
        p.bump();
        e.complete(p, ERROR);
    } else if !p.matches(EOF) {
        p.error_and_bump("expected a declaration");
    } else {
        p.error("expected a declaration");
    }
}

pub(super) fn maybe_declaration(p: &mut Parser, m: Marker) -> Result<(), Marker> {
    opt_visibility(p);

    match p.current() {
        FUNCTION_KW => {
            fn_def(p);
            m.complete(p, FUNCTION_DEF);
        }
        IMPORT_KW => import_item(p, m),
        _ => return Err(m),
    }
    Ok(())
}

pub(super) fn fn_def(p: &mut Parser) {
    assert!(p.matches(FUNCTION_KW));
    p.bump();

    name_recovery(p, DECLARATION_RECOVERY_SET.union(token_set![L_PAREN]));

    if p.matches(L_PAREN) {
        params::param_list(p);
    } else {
        p.error("expected function arguments")
    }

    if p.matches(COLON) {
        types::ascription(p);
    }

    expressions::block(p);
}

pub(super) fn import_item(p: &mut Parser, m: Marker) {
    assert!(p.matches(IMPORT_KW));
    p.bump();

    import_tree(p);
    p.expect(SEMI);

    m.complete(p, IMPORT_ITEM);
}

/// Parse an import 'tree', such as `some::path` in `use some::path;`
fn import_tree(p: &mut Parser) {
    let la = p.nth(1);
    let m = p.start();

    match (p.current(), la) {
        _ if paths::is_path_start(p) => {
            paths::import_path(p);
            match p.current() {
                AS_KW => opt_alias(p),
                COLONCOLON => {
                    p.bump();
                    match p.current() {
                        STAR => p.bump(),
                        L_CURLY => import_tree_list(p),
                        _ => p.error("expected `{` or `*`"),
                    }
                }
                _ => (),
            }
        }
        _ => {
            m.abandon(p);
            p.error_and_bump("expected on of `self`, `super` or an identifier");
            return;
        }
    }
    m.complete(p, IMPORT_TREE);
}

pub(crate) fn import_tree_list(p: &mut Parser) {
    assert!(p.matches(L_CURLY));
    let m = p.start();
    p.bump();
    while !p.matches(EOF) && !p.matches(R_CURLY) {
        import_tree(p);
        if !p.matches(R_CURLY) {
            p.expect(COMMA);
        }
    }
    p.expect(R_CURLY);
    m.complete(p, IMPORT_TREE_LIST);
}
