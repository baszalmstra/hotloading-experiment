use crate::ir::function;
use crate::IrDatabase;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::{module::Module, values::FunctionValue, OptimizationLevel};
use mun_hir::{FileId, ModuleDef};
use std::collections::HashMap;
use std::sync::Arc;

fn create_function_pass_manager(
    module: &Module,
    optimization_lvl: OptimizationLevel,
) -> PassManager {
    let pass_builder = PassManagerBuilder::create();
    pass_builder.set_optimization_level(optimization_lvl);

    let function_pass_manager = PassManager::create_for_function(module);
    pass_builder.populate_function_pass_manager(&function_pass_manager);
    function_pass_manager.initialize();

    function_pass_manager
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ModuleIR {
    /// The original source file
    pub file_id: FileId,

    /// The LLVM module that contains the IR
    pub llvm_module: Module,

    /// A mapping from HIR functions to LLVM IR values
    pub functions: HashMap<mun_hir::Function, FunctionValue>,
}

/// Generates IR for the specified file
pub(crate) fn ir_query(db: &impl IrDatabase, file_id: FileId) -> Arc<ModuleIR> {
    let llvm_module = db
        .context()
        .create_module(db.file_relative_path(file_id).as_str());

    // Generate all the function signatures
    let mut functions = HashMap::new();
    for def in db.module_data(file_id).definitions() {
        match def {
            ModuleDef::Function(f) => {
                let fun = function::gen_signature(db, *f, &llvm_module);
                functions.insert(*f, fun);
            }
            _ => {}
        }
    }

    // Generate the function bodies
    let fn_pass_manager = create_function_pass_manager(&llvm_module, db.optimization_lvl());
    for (f, value) in functions.iter() {
        function::gen_body(db, *f, *value, &llvm_module);
        fn_pass_manager.run_on_function(value);
    }

    Arc::new(ModuleIR {
        file_id,
        llvm_module,
        functions,
    })
}
