use crate::IrDatabase;
use inkwell::context::Context;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target};
use inkwell::OptimizationLevel;
use mun_hir::FileId;
use std::path::Path;

pub(crate) fn write_module_shared_object(db: &impl IrDatabase, file_id: FileId) -> bool {
    let context = db.context();
    let module = db.module_ir(file_id);

    Target::initialize_x86(&InitializationConfig::default());

    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let path = Path::new("test.o");
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine("x86_64-apple-darwin", "x86-64", "", opt, reloc, model)
        .unwrap();

    let buf = target_machine
        .write_to_file(&module, FileType::Object, &path)
        .unwrap();

    true
}
