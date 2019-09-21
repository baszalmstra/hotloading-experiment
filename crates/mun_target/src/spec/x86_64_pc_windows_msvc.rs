use crate::spec::{Target, TargetResult};

pub fn target() -> TargetResult {
    let mut base = super::apple_base::opts();
    base.cpu = "x86-64".to_string();

    Ok(Target {
        llvm_target: "x86_64-pc-windows-msvc".to_string(),
        target_os: "windows".to_string(),
        target_env: "msvc".to_string(),
        target_vendor: "pc".to_string(),
        arch: "x86_64".to_string(),
        options: base,
    })
}
