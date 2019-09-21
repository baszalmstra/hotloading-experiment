use crate::IrDatabase;
use inkwell::{
    module::Linkage,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target},
    values::{FunctionValue, StructValue},
    AddressSpace, OptimizationLevel,
};
use mun_hir::{FileId, Function};
use std::collections::HashMap;
use std::path::Path;

pub fn write_module_shared_object(db: &impl IrDatabase, file_id: FileId) -> bool {
    let context = db.context();
    let module = db.module_ir(file_id);

    // Clone the module so we can modify it safely
    let llvm_module = module.llvm_module.clone();

    // Generate the `get_symbols` method.
    gen_symbols(db, file_id, &module.functions, &llvm_module);

    Target::initialize_x86(&InitializationConfig::default());

    let target = db.target();
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

    true
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
