use super::*;

pub(crate) const LITERAL_FIRST: TokenSet = token_set![
    TRUE_KW,
    FALSE_KW,
    INT_NUMBER,
    FLOAT_NUMBER,
    STRING
];

pub(crate) fn expr_block_contents(p: &mut Parser) {
    while !p.matches(EOF) && !p.matches(R_CURLY) {
        if p.eat(SEMI) {
            continue;
        }

        stmt(p);
    }
}

pub(crate) fn block(p: &mut Parser) {
    if !p.matches(L_CURLY) {
        p.error("expected a block");
        return;
    }
    let m = p.start();
    p.bump();
    expr_block_contents(p);
    p.expect(R_CURLY);
    m.complete(p, BLOCK);
}

pub(super) fn stmt(p: &mut Parser) {
    let m = p.start();

    if p.matches(LET_KW) {
        let_stmt(p, m);
        return;
    }

    let cm = expr_stmt(p);
    let kind = cm.as_ref().map(|cm| cm.kind()).unwrap_or(ERROR);

    if p.matches(R_CURLY) {
        if let Some(cm) = cm {
            cm.undo_completion(p).abandon(p);
            m.complete(p, kind);
        } else {
            m.abandon(p);
        }
    } else {
        p.eat(SEMI);
        m.complete(p, EXPR_STMT);
    }
}

fn let_stmt(p: &mut Parser, m:Marker) {
    assert!(p.matches(LET_KW));
    p.bump();
    name(p);
    if p.matches(COLON) {
        types::ascription(p);
    }
    if p.eat(EQ ) {
        expressions::expr(p);
    }

    p.eat(SEMI);    // Semicolon at the end of statement belongs to the statement
    m.complete(p, LET_STMT);
}

fn expr(p: &mut Parser) {
    expr_bp(p, 1);
}

fn expr_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    expr_bp(p, 1)
}

fn expr_bp(p: &mut Parser, mut bp: u8) -> Option<CompletedMarker> {
    // Parse left hand side of the expression
    let mut lhs = match lhs(p) {
        Some(lhs) => lhs,
        None => return None
    };

    loop {
        let (op_bp, op) = current_op(p);
        if op_bp < bp {
            break;
        }

        let m = lhs.precede(p);
        match op {
            Op::Simple => p.bump(),
            Op::Composite(kind, n) => {
                p.bump_compound(kind, n);
            }
        }

        expr_bp(p, op_bp + 1);
        lhs = m.complete(p, BIN_EXPR);
    }

    Some(lhs)
}

enum Op {
    Simple,
    Composite(SyntaxKind, u8),
}

fn current_op(p:&Parser) -> (u8, Op)  {
    if let Some(t) = p.current2() {
        match t {
            (PLUS, EQ) => return (1, Op::Composite(PLUSEQ, 2)),
            (MINUS, EQ) => return (1, Op::Composite(MINUSEQ, 2)),
            (STAR, EQ) => return (1, Op::Composite(STAREQ, 2)),
            (SLASH, EQ) => return (1, Op::Composite(SLASHEQ, 2)),
            (CARET, EQ) => return (1, Op::Composite(CARETEQ, 2)),
            (PERCENT, EQ) => return (1, Op::Composite(PERCENTEQ, 2)),
            (LT, EQ) => return (5, Op::Composite(LTEQ, 2)),
            (GT, EQ) => return (5, Op::Composite(GTEQ, 2)),
            _ => ()
        }
    }

    let bp = match p.current() {
        EQ => 1,
        EQEQ | NEQ | LT | GT => 5,
        MINUS | PLUS => 10,
        STAR | SLASH | PERCENT => 11,
        CARET => 12,
        _ => 0,
    };
    (bp, Op::Simple)
}

fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
    let m;
    let kind = match p.current() {
        MINUS|NOT_KW => {
            m = p.start();
            p.bump();
            PREFIX_EXPR
        },
        _ => {
            let lhs = atom_expr(p)?;
            return Some(postfix_expr(p, lhs))
        }
    };
    expr_bp(p, 255);
    Some(m.complete(p, kind))
}

fn postfix_expr(p: &mut Parser, mut lhs: CompletedMarker) -> CompletedMarker {
    // TODO: Implement field access
    // TODO: Implement function call
    lhs
}

fn atom_expr(p: &mut Parser) -> Option<CompletedMarker> {
    if let Some(m) = literal(p) {
        return Some(m);
    }
    None
}

fn literal(p: &mut Parser) -> Option<CompletedMarker> {
    if !p.matches_any(LITERAL_FIRST) {
        return None;
    }
    let m = p.start();
    p.bump();
    Some(m.complete(p, LITERAL))
}