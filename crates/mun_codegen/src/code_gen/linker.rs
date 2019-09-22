use mun_target::spec;
use mun_target::spec::LinkerFlavor;
use std::ffi::OsString;
use std::path::Path;
use std::process;
use std::process::Command;

pub fn create_with_target(target: &spec::Target) -> Box<dyn Linker> {
    match target.linker_flavor {
        LinkerFlavor::Ld64 => Box::new(Ld64Linker::new(target)),
        LinkerFlavor::Msvc => Box::new(MsvcLinker::new(target)),
    }
}

pub trait Linker {
    fn add_object(&mut self, path: &Path);
    fn build_shared_object(&mut self, path: &Path);
    fn finalize(&mut self) -> process::Command;
}

struct Ld64Linker {
    cmd: process::Command,
}

struct MsvcLinker {
    cmd: process::Command,
}

impl Ld64Linker {
    fn new(target: &spec::Target) -> Self {
        let mut cmd = process::Command::new("lld");
        cmd.arg("-flavor");
        cmd.arg("ld64");

        Ld64Linker { cmd }
    }
}

impl MsvcLinker {
    fn new(target: &spec::Target) -> Self {
        let mut cmd = process::Command::new("lld");
        cmd.arg("-flavor");
        cmd.arg("link");

        MsvcLinker { cmd }
    }
}

impl Linker for Ld64Linker {
    fn add_object(&mut self, path: &Path) {
        self.cmd.arg(path);
    }

    fn build_shared_object(&mut self, path: &Path) {
        // Link as dynamic library
        self.cmd.arg("-dylib");
        self.cmd.arg("-lsystem");

        // Specify output path
        self.cmd.arg("-o");
        self.cmd.arg(path);
    }

    fn finalize(&mut self) -> process::Command {
        ::std::mem::replace(&mut self.cmd, Command::new(""))
    }
}

impl Linker for MsvcLinker {
    fn add_object(&mut self, path: &Path) {
        self.cmd.arg(path);
    }

    fn build_shared_object(&mut self, path: &Path) {
        self.cmd.arg("/DLL");
        self.cmd.arg("/NOENTRY");
        self.cmd.arg("/EXPORT:get_symbols");
    }

    fn finalize(&mut self) -> process::Command {
        ::std::mem::replace(&mut self.cmd, Command::new(""))
    }
}
