pub use inkwell;

use inkwell::{
    values, builder, module
};

use mun_syntax::ast;
use mun_errors::Diagnostic;
use std::collections::HashMap;
use mun_syntax::ast::FunctionDefOwner;

pub struct CodeGenResult {
    pub functions: HashMap<String, values::FunctionValue>,
    pub diagnostics: Vec<Diagnostic>
}

pub fn code_gen<'a>(sourceFile:&ast::SourceFile, module: &'a module::Module, builder: &'a builder::Builder) -> CodeGenResult {
    let builder = Context::new(module, builder);

    for function in sourceFile.functions() {

    }

    builder.complete()
}

struct Context<'a> {
    pub builder: &'a builder::Builder,
    pub module: &'a module::Module,
    pub diagnostics: Vec<Diagnostic>,
    pub functions: HashMap<String, values::FunctionValue>
}

impl<'a> Context<'a> {
    pub fn new(module: &'a module::Module, builder: &'a builder::Builder) -> Context<'a> {
        Context {
            builder, module, diagnostics: Vec::new(), functions: HashMap::new()
        }
    }

    pub fn complete(self) -> CodeGenResult {
        CodeGenResult {
            diagnostics: self.diagnostics,
            functions: self.functions
        }
    }
}
