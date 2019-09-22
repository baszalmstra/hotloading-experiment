use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let llvm_sys_prefix = env::var("LLVM_SYS_70_PREFIX").unwrap();
    let lld_path = Path::new(&llvm_sys_prefix)
        .join(format!("bin/lld{}", std::env::consts::EXE_SUFFIX))
        .canonicalize()
        .expect("unable to locate 'lld' are you sure its available?");
    let lld_output_path = Path::new(&out_dir)
        .join("../../..")
        .canonicalize()
        .unwrap()
        .join(format!("lld{}", std::env::consts::EXE_SUFFIX));
    fs::copy(lld_path, lld_output_path).unwrap();
}