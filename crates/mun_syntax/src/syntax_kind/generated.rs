// This file is automatically generated based on the file `./generated.rs.tera` when `cargo gen-syntax` is run
// Do not edit manually

#![allow(bad_style, missing_docs, unreachable_pub)]
#![cfg_attr(rustfmt, rustfmt_skip)]
use super::SyntaxInfo;

/// The kind of syntax node, e.g. `IDENT`, `USE_KW`, or `STRUCT_DEF`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // Technical SyntaxKinds: they appear temporally during parsing,
    // but never end up in the final tree
    #[doc(hidden)]
    TOMBSTONE,
    #[doc(hidden)]
    EOF,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    CARET,
    HASH,
    DOT,
    LT,
    GT,
    EQ,
    L_PAREN,
    R_PAREN,
    L_CURLY,
    R_CURLY,
    L_BRACKET,
    R_BRACKET,
    SEMI,
    COLON,
    COMMA,
    UNDERSCORE,
    EQEQ,
    NEQ,
    LTEQ,
    GTEQ,
    DOTDOT,
    DOTDOTDOT,
    PLUSEQ,
    MINUSEQ,
    STAREQ,
    SLASHEQ,
    CARETEQ,
    PERCENTEQ,
    DOTDOTEQ,
    COLONCOLON,
    AND_KW,
    BREAK_KW,
    DO_KW,
    ELSE_KW,
    FALSE_KW,
    FOR_KW,
    FUNCTION_KW,
    IF_KW,
    IN_KW,
    NIL_KW,
    NOT_KW,
    OR_KW,
    RETURN_KW,
    THEN_KW,
    TRUE_KW,
    WHILE_KW,
    LET_KW,
    MUT_KW,
    CLASS_KW,
    PUBLIC_KW,
    PROTECTED_KW,
    PRIVATE_KW,
    EXPORT_KW,
    INT_NUMBER,
    FLOAT_NUMBER,
    STRING,
    ERROR,
    IDENT,
    WHITESPACE,
    COMMENT,
    SOURCE_FILE,
    FUNCTION_DEF,
    VISIBILITY,
    PARAM_LIST,
    PARAM,
    LET_STMT,
    EXPR_STMT,
    PREFIX_EXPR,
    LITERAL,
    BIN_EXPR,
    NAME,
    NAME_REF,
    BLOCK,
    // Technical kind so that we can cast from u16 safely
    #[doc(hidden)]
    __LAST,
}
use self::SyntaxKind::*;

impl From<u16> for SyntaxKind {
    fn from(d: u16) -> SyntaxKind {
        assert!(d <= (__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(d) }
    }
}

impl From<SyntaxKind> for u16 {
    fn from(k: SyntaxKind) -> u16 {
        k as u16
    }
}

impl SyntaxKind {
    pub fn is_keyword(self) -> bool {
        match self {
            | AND_KW
            | BREAK_KW
            | DO_KW
            | ELSE_KW
            | FALSE_KW
            | FOR_KW
            | FUNCTION_KW
            | IF_KW
            | IN_KW
            | NIL_KW
            | NOT_KW
            | OR_KW
            | RETURN_KW
            | THEN_KW
            | TRUE_KW
            | WHILE_KW
            | LET_KW
            | MUT_KW
            | CLASS_KW
            | PUBLIC_KW
            | PROTECTED_KW
            | PRIVATE_KW
            | EXPORT_KW
                => true,
            _ => false
        }
    }

    pub fn is_symbol(self) -> bool {
            match self {
                | PLUS
                | MINUS
                | STAR
                | SLASH
                | PERCENT
                | CARET
                | HASH
                | DOT
                | LT
                | GT
                | EQ
                | L_PAREN
                | R_PAREN
                | L_CURLY
                | R_CURLY
                | L_BRACKET
                | R_BRACKET
                | SEMI
                | COLON
                | COMMA
                | UNDERSCORE
                | EQEQ
                | NEQ
                | LTEQ
                | GTEQ
                | DOTDOT
                | DOTDOTDOT
                | PLUSEQ
                | MINUSEQ
                | STAREQ
                | SLASHEQ
                | CARETEQ
                | PERCENTEQ
                | DOTDOTEQ
                | COLONCOLON
                    => true,
                _ => false
            }
    }

    pub fn is_literal(self) -> bool {
            match self {
                | INT_NUMBER
                | FLOAT_NUMBER
                | STRING
                    => true,
                _ => false
            }
    }

    pub(crate) fn info(self) -> &'static SyntaxInfo {
            match self {
                PLUS => &SyntaxInfo { name: "PLUS" },
                MINUS => &SyntaxInfo { name: "MINUS" },
                STAR => &SyntaxInfo { name: "STAR" },
                SLASH => &SyntaxInfo { name: "SLASH" },
                PERCENT => &SyntaxInfo { name: "PERCENT" },
                CARET => &SyntaxInfo { name: "CARET" },
                HASH => &SyntaxInfo { name: "HASH" },
                DOT => &SyntaxInfo { name: "DOT" },
                LT => &SyntaxInfo { name: "LT" },
                GT => &SyntaxInfo { name: "GT" },
                EQ => &SyntaxInfo { name: "EQ" },
                L_PAREN => &SyntaxInfo { name: "L_PAREN" },
                R_PAREN => &SyntaxInfo { name: "R_PAREN" },
                L_CURLY => &SyntaxInfo { name: "L_CURLY" },
                R_CURLY => &SyntaxInfo { name: "R_CURLY" },
                L_BRACKET => &SyntaxInfo { name: "L_BRACKET" },
                R_BRACKET => &SyntaxInfo { name: "R_BRACKET" },
                SEMI => &SyntaxInfo { name: "SEMI" },
                COLON => &SyntaxInfo { name: "COLON" },
                COMMA => &SyntaxInfo { name: "COMMA" },
                UNDERSCORE => &SyntaxInfo { name: "UNDERSCORE" },
                EQEQ => &SyntaxInfo { name: "EQEQ" },
                NEQ => &SyntaxInfo { name: "NEQ" },
                LTEQ => &SyntaxInfo { name: "LTEQ" },
                GTEQ => &SyntaxInfo { name: "GTEQ" },
                DOTDOT => &SyntaxInfo { name: "DOTDOT" },
                DOTDOTDOT => &SyntaxInfo { name: "DOTDOTDOT" },
                PLUSEQ => &SyntaxInfo { name: "PLUSEQ" },
                MINUSEQ => &SyntaxInfo { name: "MINUSEQ" },
                STAREQ => &SyntaxInfo { name: "STAREQ" },
                SLASHEQ => &SyntaxInfo { name: "SLASHEQ" },
                CARETEQ => &SyntaxInfo { name: "CARETEQ" },
                PERCENTEQ => &SyntaxInfo { name: "PERCENTEQ" },
                DOTDOTEQ => &SyntaxInfo { name: "DOTDOTEQ" },
                COLONCOLON => &SyntaxInfo { name: "COLONCOLON" },
                AND_KW => &SyntaxInfo { name: "AND_KW" },
                BREAK_KW => &SyntaxInfo { name: "BREAK_KW" },
                DO_KW => &SyntaxInfo { name: "DO_KW" },
                ELSE_KW => &SyntaxInfo { name: "ELSE_KW" },
                FALSE_KW => &SyntaxInfo { name: "FALSE_KW" },
                FOR_KW => &SyntaxInfo { name: "FOR_KW" },
                FUNCTION_KW => &SyntaxInfo { name: "FUNCTION_KW" },
                IF_KW => &SyntaxInfo { name: "IF_KW" },
                IN_KW => &SyntaxInfo { name: "IN_KW" },
                NIL_KW => &SyntaxInfo { name: "NIL_KW" },
                NOT_KW => &SyntaxInfo { name: "NOT_KW" },
                OR_KW => &SyntaxInfo { name: "OR_KW" },
                RETURN_KW => &SyntaxInfo { name: "RETURN_KW" },
                THEN_KW => &SyntaxInfo { name: "THEN_KW" },
                TRUE_KW => &SyntaxInfo { name: "TRUE_KW" },
                WHILE_KW => &SyntaxInfo { name: "WHILE_KW" },
                LET_KW => &SyntaxInfo { name: "LET_KW" },
                MUT_KW => &SyntaxInfo { name: "MUT_KW" },
                CLASS_KW => &SyntaxInfo { name: "CLASS_KW" },
                PUBLIC_KW => &SyntaxInfo { name: "PUBLIC_KW" },
                PROTECTED_KW => &SyntaxInfo { name: "PROTECTED_KW" },
                PRIVATE_KW => &SyntaxInfo { name: "PRIVATE_KW" },
                EXPORT_KW => &SyntaxInfo { name: "EXPORT_KW" },
                INT_NUMBER => &SyntaxInfo { name: "INT_NUMBER" },
                FLOAT_NUMBER => &SyntaxInfo { name: "FLOAT_NUMBER" },
                STRING => &SyntaxInfo { name: "STRING" },
                ERROR => &SyntaxInfo { name: "ERROR" },
                IDENT => &SyntaxInfo { name: "IDENT" },
                WHITESPACE => &SyntaxInfo { name: "WHITESPACE" },
                COMMENT => &SyntaxInfo { name: "COMMENT" },
                SOURCE_FILE => &SyntaxInfo { name: "SOURCE_FILE" },
                FUNCTION_DEF => &SyntaxInfo { name: "FUNCTION_DEF" },
                VISIBILITY => &SyntaxInfo { name: "VISIBILITY" },
                PARAM_LIST => &SyntaxInfo { name: "PARAM_LIST" },
                PARAM => &SyntaxInfo { name: "PARAM" },
                LET_STMT => &SyntaxInfo { name: "LET_STMT" },
                EXPR_STMT => &SyntaxInfo { name: "EXPR_STMT" },
                PREFIX_EXPR => &SyntaxInfo { name: "PREFIX_EXPR" },
                LITERAL => &SyntaxInfo { name: "LITERAL" },
                BIN_EXPR => &SyntaxInfo { name: "BIN_EXPR" },
                NAME => &SyntaxInfo { name: "NAME" },
                NAME_REF => &SyntaxInfo { name: "NAME_REF" },
                BLOCK => &SyntaxInfo { name: "BLOCK" },
                TOMBSTONE => &SyntaxInfo { name: "TOMBSTONE" },
                EOF => &SyntaxInfo { name: "EOF" },
                __LAST => &SyntaxInfo { name: "__LAST" },
            }
        }

    pub fn from_keyword(ident: &str) -> Option<SyntaxKind> {
            let kw = match ident {
                "and" => AND_KW,
                "break" => BREAK_KW,
                "do" => DO_KW,
                "else" => ELSE_KW,
                "false" => FALSE_KW,
                "for" => FOR_KW,
                "function" => FUNCTION_KW,
                "if" => IF_KW,
                "in" => IN_KW,
                "nil" => NIL_KW,
                "not" => NOT_KW,
                "or" => OR_KW,
                "return" => RETURN_KW,
                "then" => THEN_KW,
                "true" => TRUE_KW,
                "while" => WHILE_KW,
                "let" => LET_KW,
                "mut" => MUT_KW,
                "class" => CLASS_KW,
                "public" => PUBLIC_KW,
                "protected" => PROTECTED_KW,
                "private" => PRIVATE_KW,
                "export" => EXPORT_KW,
                _ => return None,
            };
            Some(kw)
    }

    pub fn from_char(c: char) -> Option<SyntaxKind> {
            let tok = match c {
                '+' => PLUS,
                '-' => MINUS,
                '*' => STAR,
                '/' => SLASH,
                '%' => PERCENT,
                '^' => CARET,
                '#' => HASH,
                '.' => DOT,
                '<' => LT,
                '>' => GT,
                '=' => EQ,
                '(' => L_PAREN,
                ')' => R_PAREN,
                '{' => L_CURLY,
                '}' => R_CURLY,
                '[' => L_BRACKET,
                ']' => R_BRACKET,
                ';' => SEMI,
                ':' => COLON,
                ',' => COMMA,
                '_' => UNDERSCORE,
                _ => return None,
            };
            Some(tok)
        }
}



