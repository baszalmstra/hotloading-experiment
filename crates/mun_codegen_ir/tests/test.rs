extern crate mun_codegen_ir;

use mun_hir::{
    salsa, FileId, HirDisplay, Module, ModuleDef, PackageInput, RelativePathBuf, SourceDatabase,
};
use std::path::{Path, PathBuf};
use std::error::Error;
use std::sync::Arc;
use test_utils::{dir_tests, project_dir};
use crate::mun_codegen_ir::IrDatabase;

#[salsa::database(
mun_hir::SourceDatabaseStorage,
mun_hir::DefDatabaseStorage,
mun_hir::HirDatabaseStorage,
mun_codegen_ir::IrDatabaseStorage
)]
#[derive(Default, Debug)]
struct MockDatabase {
    runtime: salsa::Runtime<MockDatabase>,
}

impl salsa::Database for MockDatabase {
    fn salsa_runtime(&self) -> &salsa::Runtime<MockDatabase> {
        &self.runtime
    }
}

fn test_data_dir() -> PathBuf {
    project_dir().join("crates/mun_codegen_ir/tests/data/")
}

#[test]
fn ir_tests() {
    dir_tests(&test_data_dir(), &["ir"], |text, path| {
        let mut db = MockDatabase::default();
        let file_id = FileId(0);
        dbg!(path);
        db.set_file_relative_path(file_id, RelativePathBuf::from("main.mun"));
        db.set_file_text(file_id, Arc::new(text.to_string()));
        let mut package_input = PackageInput::default();
        package_input.add_module(file_id);
        db.set_package_input(Arc::new(package_input));

        let context = mun_codegen_ir::Context::create();
        db.set_context(Arc::new(context));

        format!("{}", db.module_ir(file_id).print_to_string().to_string())
    });
}