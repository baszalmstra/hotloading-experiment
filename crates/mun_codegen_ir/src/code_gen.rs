use crate::IrDatabase;
use inkwell::{
    module::Linkage,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target},
    values::{FunctionValue, StructValue},
    AddressSpace, OptimizationLevel,
};
use mun_hir::{FileId, Function};
use mun_target::spec;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

mod linker;
pub mod symbols;

pub fn write_module_shared_object(db: &impl IrDatabase, file_id: FileId) -> bool {
    let context = db.context();
    let module = db.module_ir(file_id);
    let target = db.target();

    // Clone the module so we can modify it safely
    let llvm_module = module.llvm_module.clone();

    // Generate startup procedure if not found
    generate_startup_proc(&target, &llvm_module);

    // Generate the `get_symbols` method.
    symbols::gen_symbols(db, file_id, &module.functions, &llvm_module);

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

    let buf = target_machine
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



fn generate_startup_proc(target: &spec::Target, module: &Module) {
//    if target.target_os == "windows" {
//        generate_dll_main_proc(module);
//    }
}

fn generate_dll_main_proc(module: &Module) {
    let context = module.get_context();
    let startup_type_fn = context.void_type().fn_type(&[], false);
    let startup_fn = module.add_function(
        "_DllMainCRTStartup",
        startup_type_fn,
        Some(Linkage::DLLExport),
    );

    let builder = context.create_builder();
    let body_ir = startup_fn.append_basic_block("body");
    builder.position_at_end(&body_ir);
    builder.build_return(None);
}
