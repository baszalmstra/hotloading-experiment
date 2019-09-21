use crate::spec::{Target, TargetResult};

pub fn target() -> TargetResult {
    let mut base = super::apple_base::opts();

    Ok(Target {
        llvm_target: "x86_64_apple_darwin".to_string(),
        target_os: "macos".to_string(),
        target_env: String::new(),
        target_vendor: "apple".to_string(),
        arch: "x86_64".to_string(),
        options: base,
    })
}
