///! This library contains the code required to go from source code to binaries.
mod diagnostic;

use crate::diagnostic::Emit;
use failure::Error;
use mun_codegen_ir::IrDatabase;
use mun_errors::{Diagnostic, Level};
use mun_hir::diagnostics::{Diagnostic as HirDiagnostic, DiagnosticSink};
use mun_hir::{
    salsa, FileId, HirDisplay, Module, ModuleDef, PackageInput, RelativePathBuf, SourceDatabase,
};
use mun_syntax::ast::AstNode;
use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use termcolor::{ColorChoice, StandardStream};

#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub input: PathBuf,
}

#[salsa::database(
    mun_hir::SourceDatabaseStorage,
    mun_hir::DefDatabaseStorage,
    mun_hir::HirDatabaseStorage,
    mun_codegen_ir::IrDatabaseStorage
)]
#[derive(Debug)]
pub struct CompilerDatabase {
    events: Mutex<Option<Vec<salsa::Event<CompilerDatabase>>>>,
    runtime: salsa::Runtime<CompilerDatabase>,
}

impl salsa::Database for CompilerDatabase {
    fn salsa_runtime(&self) -> &salsa::Runtime<CompilerDatabase> {
        &self.runtime
    }
    fn salsa_event(&self, event: impl Fn() -> salsa::Event<CompilerDatabase>) {
        let mut events = self.events.lock().unwrap();
        if let Some(events) = &mut *events {
            events.push(event());
        }
    }
}

/// Implements the ability to retrieve query results in a closure.
impl CompilerDatabase {
    pub fn log(&self, f: impl FnOnce()) -> Vec<salsa::Event<CompilerDatabase>> {
        *self.events.lock().unwrap() = Some(Vec::new());
        f();
        self.events.lock().unwrap().take().unwrap()
    }

    pub fn log_executed(&self, f: impl FnOnce()) -> Vec<String> {
        let events = self.log(f);
        events
            .into_iter()
            .filter_map(|e| match e.kind {
                // This pretty horrible, but `Debug` is the only way to inspect
                // QueryDescriptor at the moment.
                salsa::EventKind::WillExecute { database_key } => {
                    Some(format!("{:#?}", database_key.kind))
                }
                _ => None,
            })
            .collect()
    }
}

impl CompilerDatabase {
    fn from_file(path: &Path) -> Result<(CompilerDatabase, FileId), Error> {
        let mut db = CompilerDatabase {
            runtime: salsa::Runtime::default(),
            events: Mutex::new(Some(Vec::new())),
        };
        let file_id = FileId(0);
        db.set_file_relative_path(file_id, RelativePathBuf::from_path(path).unwrap());
        db.set_file_text(file_id, Arc::new(std::fs::read_to_string(path)?));
        let mut package_input = PackageInput::default();
        package_input.add_module(file_id);
        db.set_package_input(Arc::new(package_input));

        let context = mun_codegen_ir::Context::create();
        db.set_context(Arc::new(context));

        Ok((db, file_id))
    }
}

fn diagnostics(db: &CompilerDatabase, file_id: FileId) -> Vec<Diagnostic> {
    let parse = db.parse(file_id);
    let mut result = Vec::new();

    result.extend(parse.errors().iter().map(|err| Diagnostic {
        level: Level::Error,
        loc: err.location(),
        message: format!("Syntax Error: {}", err),
    }));

    let result = RefCell::new(result);
    let mut sink = DiagnosticSink::new(|d| {
        result.borrow_mut().push(Diagnostic {
            level: Level::Error,
            loc: d.highlight_range().into(),
            message: d.message(),
        });
    })
    .on::<mun_hir::diagnostics::UnresolvedValue, _>(|d| {
        let text = d.expr.to_node(&parse.tree().syntax()).text().to_string();
        result.borrow_mut().push(Diagnostic {
            level: Level::Error,
            loc: d.highlight_range().into(),
            message: format!("could not find value `{}` in this scope", text),
        });
    })
    .on::<mun_hir::diagnostics::UnresolvedType, _>(|d| {
        let text = d
            .type_ref
            .to_node(&parse.tree().syntax())
            .syntax()
            .text()
            .to_string();
        result.borrow_mut().push(Diagnostic {
            level: Level::Error,
            loc: d.highlight_range().into(),
            message: format!("could not find type `{}` in this scope", text),
        });
    })
    .on::<mun_hir::diagnostics::MismatchedType, _>(|d| {
        result.borrow_mut().push(Diagnostic {
            level: Level::Error,
            loc: d.highlight_range().into(),
            message: format!("expected `{}`, found `{}`", d.expected.display(db), d.found.display(db)),
        });
    });

    if let Some(module) = Module::package_modules(db)
        .iter()
        .find(|m| m.file_id() == file_id)
    {
        module.diagnostics(db, &mut sink)
    }

    drop(sink);
    result.into_inner()
}

pub fn main(options: CompilerOptions) -> Result<(), failure::Error> {
    let (db, file_id) = CompilerDatabase::from_file(&options.input)?;

    let diagnostics = diagnostics(&db, file_id);
    if !diagnostics.is_empty() {
        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        for diagnostic in diagnostics {
            diagnostic.emit(&mut writer, &db, file_id)?;
        }
        return Ok(());
    }

    println!("{}", db.module_ir(file_id).print_to_string().to_string());

    Ok(())
}
