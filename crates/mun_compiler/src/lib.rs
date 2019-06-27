///! This library contains the code required to go from source code to binaries.

mod diagnostic;

use std::path::{PathBuf, Path};
use mun_hir::{salsa, FileId, SourceDatabase, DefDatabase};
use failure::Error;
use std::sync::{Arc, Mutex};
use colored::Colorize;

#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub input: PathBuf,
}

#[salsa::database(mun_hir::SourceDatabaseStorage, mun_hir::DefDatabaseStorage)]
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
    fn from_file(path:&Path) -> Result<(CompilerDatabase, FileId), Error> {
        let mut db = CompilerDatabase { runtime: salsa::Runtime::default(), events: Mutex::new(Some(Vec::new())) };
        let file_id = FileId(0);
        db.set_file_text(file_id, Arc::new(std::fs::read_to_string(path)?));
        Ok((db, file_id))
    }
}

pub fn main(options: CompilerOptions) -> Result<(), failure::Error> {
    let (db, file_id) = CompilerDatabase::from_file(&options.input)?;

//    println!("{}", "Queries:".white());
//    for l in db.log_executed(|| {
//        db.ast_id_map(file_id);
//    }).into_iter() {
//        println!("{}", l);
//    }
//    println!();

//    // Parse the contents of the file and memoize the results
//    let line_index = db.line_index(file_id);
//    let source = db.parse(file_id);
//    let maps = db.ast_id_map(file_id);

//    // Check if there are parser errors
//    println!("{}", "Syntax Tree:".white());
//    let errors = source.errors();
//    if errors.len() > 0 {
//        // TODO: Improve errors
//        for err in errors {
//            Into::<Diagnostic>::into(err)
//                .emit(&line_index);
//        }
//        return Ok(())
//    }
//
//  println!("{}", source.syntax().debug_dump());

//    for raw_item in .items() {
//        println!("{:?}", raw_item);
//    }
    let query_log = db.log_executed(|| {
        println!("{:#?}", db.raw_items(file_id));
    });

    println!("\n{}", "Queries:".white());
    for l in query_log.into_iter() {
        println!("{}", l);
    }

    Ok(())
}