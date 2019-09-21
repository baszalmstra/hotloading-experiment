use crate::spec::TargetOptions;

pub fn opts() -> TargetOptions {
    TargetOptions {
        dll_prefix: "lib".to_string(),
        dll_suffix: ".dylib".to_string(),
        ..Default::default()
    }
}
