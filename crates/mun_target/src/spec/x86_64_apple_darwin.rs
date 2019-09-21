use crate::spec::{LinkerFlavor, Target, TargetResult};

pub fn target() -> TargetResult {
    let mut base = super::apple_base::opts();
    base.cpu = "core2".to_string();

    Ok(Target {
        llvm_target: "x86_64-apple-darwin".to_string(),
        target_os: "macos".to_string(),
        target_env: String::new(),
        target_vendor: "apple".to_string(),
        arch: "x86_64".to_string(),
        linker_flavor: LinkerFlavor::Ld64,
        options: base,
    })
}
