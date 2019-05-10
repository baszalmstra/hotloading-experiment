// This file is automatically generated based on the file `./generated.rs.tera` when `cargo gen-syntax` is run
// Do not edit manually

//! This module contains auto-generated Rust AST. Like `SyntaxNode`s, AST nodes
//! are generic over ownership: `X<'a>` things are `Copy` references, `XNode`
//! are Arc-based. You can switch between the two variants using `.owned` and
//! `.borrowed` functions. Most of the code works with borrowed mode, and only
//! this mode has all AST accessors.

use rowan::TransparentNewType;

use crate::{
    SyntaxNode, SyntaxKind::*,
    syntax_node::{TreeArc},
    ast::{self, AstNode},
};


// Block
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Block {
    pub(crate) syntax: SyntaxNode,
}

unsafe impl TransparentNewType for Block {
    type Repr = rowan::SyntaxNode;
}

impl AstNode for Block {
    fn cast(syntax: &SyntaxNode) -> Option<&Self> {
        match syntax.kind() {
            BLOCK => Some(Block::from_repr(syntax.into_repr())),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}

impl ToOwned for Block {
    type Owned = TreeArc<Block>;
    fn to_owned(&self) -> TreeArc<Block> { TreeArc::cast(self.syntax.to_owned()) }
}


impl Block {
}


// FunctionDef
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FunctionDef {
    pub(crate) syntax: SyntaxNode,
}

unsafe impl TransparentNewType for FunctionDef {
    type Repr = rowan::SyntaxNode;
}

impl AstNode for FunctionDef {
    fn cast(syntax: &SyntaxNode) -> Option<&Self> {
        match syntax.kind() {
            FUNCTION_DEF => Some(FunctionDef::from_repr(syntax.into_repr())),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}

impl ToOwned for FunctionDef {
    type Owned = TreeArc<FunctionDef>;
    fn to_owned(&self) -> TreeArc<FunctionDef> { TreeArc::cast(self.syntax.to_owned()) }
}


impl FunctionDef {
}


// ModuleItem
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ModuleItem {
    pub(crate) syntax: SyntaxNode,
}
unsafe impl TransparentNewType for ModuleItem {
    type Repr = rowan::SyntaxNode;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleItemKind<'a>{
    FunctionDef(&'a FunctionDef),
}
impl<'a> From<&'a FunctionDef> for &'a ModuleItem {
    fn from(n: &'a FunctionDef) -> &'a ModuleItem {
        ModuleItem::cast(&n.syntax).unwrap()
    }
}

impl AstNode for ModuleItem {
    fn cast(syntax: &SyntaxNode) -> Option<&Self> {
        match syntax.kind() {
            
            | FUNCTION_DEF => Some(ModuleItem::from_repr(syntax.into_repr())),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}

impl ToOwned for ModuleItem {
    type Owned = TreeArc<ModuleItem>;
    fn to_owned(&self) -> TreeArc<ModuleItem> { TreeArc::cast(self.syntax.to_owned()) }
}

impl ModuleItem {
    pub fn kind(&self) -> ModuleItemKind {
        match self.syntax.kind() {
            FUNCTION_DEF => ModuleItemKind::FunctionDef(FunctionDef::cast(&self.syntax).unwrap()),
            _ => unreachable!(),
        }
    }
}

impl ModuleItem {
}


// Name
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Name {
    pub(crate) syntax: SyntaxNode,
}

unsafe impl TransparentNewType for Name {
    type Repr = rowan::SyntaxNode;
}

impl AstNode for Name {
    fn cast(syntax: &SyntaxNode) -> Option<&Self> {
        match syntax.kind() {
            NAME => Some(Name::from_repr(syntax.into_repr())),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}

impl ToOwned for Name {
    type Owned = TreeArc<Name>;
    fn to_owned(&self) -> TreeArc<Name> { TreeArc::cast(self.syntax.to_owned()) }
}


impl Name {
}


// SourceFile
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SourceFile {
    pub(crate) syntax: SyntaxNode,
}

unsafe impl TransparentNewType for SourceFile {
    type Repr = rowan::SyntaxNode;
}

impl AstNode for SourceFile {
    fn cast(syntax: &SyntaxNode) -> Option<&Self> {
        match syntax.kind() {
            SOURCE_FILE => Some(SourceFile::from_repr(syntax.into_repr())),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.syntax }
}

impl ToOwned for SourceFile {
    type Owned = TreeArc<SourceFile>;
    fn to_owned(&self) -> TreeArc<SourceFile> { TreeArc::cast(self.syntax.to_owned()) }
}


impl ast::ModuleItemOwner for SourceFile {}
impl ast::FunctionDefOwner for SourceFile {}
impl SourceFile {
}

