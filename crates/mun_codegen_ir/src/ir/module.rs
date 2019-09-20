use crate::ir::function;
use crate::IrDatabase;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::{
    module::{Linkage, Module},
    values::{FunctionValue, StructValue},
    AddressSpace, OptimizationLevel,
};
use mun_hir::{FileId, Function, ModuleDef};
use std::collections::HashMap;

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

pub(crate) fn ir_query(db: &impl IrDatabase, file_id: FileId) -> Module {
    let module = db
        .context()
        .create_module(db.file_relative_path(file_id).as_str());

    let fn_pass_manager = create_function_pass_manager(&module, db.optimization_lvl());

    let mod_data = db.module_data(file_id);

    let mut function_values = HashMap::new();
    for def in mod_data.definitions() {
        match def {
            ModuleDef::Function(f) => {
                let fun = function::gen_signature(db, *f, &module);
                function_values.insert(*f, fun);
            }
            _ => {}
        }
    }

    for (f, value) in function_values.iter() {
        function::gen_body(db, *f, *value, &module);
        fn_pass_manager.run_on_function(value);
    }

    gen_symbols(db, file_id, &function_values, &module);

    module
}

fn gen_symbols(
    db: &impl IrDatabase,
    file_id: FileId,
    function_map: &HashMap<Function, FunctionValue>,
    module: &Module,
) {
    let context = module.get_context();
    let str_type = context.i8_type().ptr_type(AddressSpace::Const);

    let guid_type = context.i8_type().array_type(16);
    let privacy_type = context.i8_type();

    let type_info_type = context.opaque_struct_type("TypeInfo");
    type_info_type.set_body(
        &[
            guid_type.into(), // guid
            str_type.into(),  // name
        ],
        false,
    );

    let method_info_type = context.opaque_struct_type("MethodInfo");
    method_info_type.set_body(
        &[
            str_type.into(),                                          // name
            type_info_type.ptr_type(AddressSpace::Const).into(),      // arg_types
            type_info_type.ptr_type(AddressSpace::Const).into(),      // return_type
            context.void_type().ptr_type(AddressSpace::Const).into(), // fn_ptr
            context.i16_type().into(),                                // num_arg_types
            privacy_type.into(),                                      // privacy
        ],
        false,
    );

    let module_info_type = context.opaque_struct_type("ModuleInfo");
    module_info_type.set_body(
        &[
            method_info_type.ptr_type(AddressSpace::Const).into(),
            context.i32_type().into(),
        ],
        false,
    );

    let method_infos: Vec<StructValue> = function_map
        .iter()
        .map(|(f, value)| {
            let name_const = context.const_string(&f.name(db).to_string(), true);
            let name_str = module.add_global(name_const.get_type(), None, "");
            name_str.set_linkage(Linkage::Internal);
            name_str.set_initializer(&name_const);
            context.const_struct(
                &[
                    name_str.as_pointer_value().into(),
                    type_info_type
                        .ptr_type(AddressSpace::Const)
                        .const_null()
                        .into(),
                    type_info_type
                        .ptr_type(AddressSpace::Const)
                        .const_null()
                        .into(),
                    value.as_global_value().as_pointer_value().into(),
                    context.i16_type().const_int(0, false).into(),
                    context.i8_type().const_int(0, false).into(),
                ],
                false,
            )
        })
        .collect();

    let method_info_array = method_info_type.const_array(&method_infos);
    let method_info = module.add_global(method_info_array.get_type(), None, "");
    method_info.set_linkage(Linkage::Internal);
    method_info.set_initializer(&method_info_array);
    let module_info = context.const_struct(
        &[
            method_info.as_pointer_value().into(),
            context
                .i32_type()
                .const_int(method_infos.len() as u64, false)
                .into(),
        ],
        false,
    );

    let get_symbols_type = module_info.get_type().fn_type(&[], false);
    let get_symbols_fn =
        module.add_function("get_symbols", get_symbols_type, Some(Linkage::DLLExport));

    let builder = db.context().create_builder();
    let body_ir = get_symbols_fn.append_basic_block("body");
    builder.position_at_end(&body_ir);

    builder.build_return(Some(&module_info));
}
