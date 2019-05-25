///! This library contains the code required to go from source code to binaries.

mod diagnostic;

use std::path::{PathBuf, Path};
use mun_syntax::ast;
use mun_errors::Diagnostic;
use diagnostic::Emit;
use mun_syntax::ast::AstNode;
use mun_hir::{salsa, FileId, SourceDatabase};
use failure::Error;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub input: PathBuf,
}

#[salsa::database(mun_hir::SourceDatabaseStorage)]
#[derive(Debug)]
pub struct CompilerDatabase {
    runtime: salsa::Runtime<CompilerDatabase>,
}

impl salsa::Database for CompilerDatabase {
    fn salsa_runtime(&self) -> &salsa::Runtime<CompilerDatabase> {
        &self.runtime
    }
}

impl CompilerDatabase {
    fn from_file(path:&Path) -> Result<(CompilerDatabase, FileId), Error> {
        let mut db = CompilerDatabase { runtime: salsa::Runtime::default() };
        let file_id = FileId(0);
        db.set_file_text(file_id, Arc::new(std::fs::read_to_string(path)?));
        Ok((db, file_id))
    }
}

pub fn main(options: CompilerOptions) -> Result<(), failure::Error> {
    let (db, file_id) = CompilerDatabase::from_file(&options.input)?;

    // Parse the contents of the file and memoize the results
    let line_index = db.line_index(file_id);
    let source = db.parse(file_id);

    // Check if there are parser errors
    let errors = source.errors();
    if errors.len() > 0 {
        // TODO: Improve errors
        for err in errors {
            Into::<Diagnostic>::into(err)
                .emit(&line_index);
        }
        return Ok(())
    }

    println!("{}", source.syntax().debug_dump());


    Ok(())
}