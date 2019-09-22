use crate::IrDatabase;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target};
use mun_hir::FileId;
use mun_target::spec;
use std::path::Path;

mod linker;
pub mod symbols;

pub fn write_module_shared_object(db: &impl IrDatabase, file_id: FileId) -> bool {
    let module = db.module_ir(file_id);
    let target = db.target();

    // Clone the module so we can modify it safely
    let llvm_module = module.llvm_module.clone();

    // Generate the `get_symbols` method.
    symbols::gen_symbols(db, &module.functions, &llvm_module);

    Target::initialize_x86(&InitializationConfig::default());

    let path = Path::new("test.o");
    let llvm_target = Target::from_triple(&target.llvm_target).unwrap();
    let target_machine = llvm_target
        .create_target_machine(
            &target.llvm_target,
            &target.options.cpu,
            &target.options.features,
            db.optimization_lvl(),
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();

    target_machine
        .write_to_file(&llvm_module, FileType::Object, &path)
        .unwrap();

    link_obj_to_shared_object(&target, &path);

    true
}

fn link_obj_to_shared_object(target: &spec::Target, path: &Path) {
    let mut linker = linker::create_with_target(target);
    linker.add_object(path);

    // Determine output file
    let output_path = format!(
        "{}test{}",
        target.options.dll_prefix, target.options.dll_suffix
    );
    linker.build_shared_object(Path::new(&output_path));

    let mut cmd = linker.finalize();
    cmd.spawn().unwrap().wait().unwrap();
}
