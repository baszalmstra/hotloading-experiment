#[macro_use]
extern crate failure;

use clap::{App, AppSettings, Arg, ArgMatches, SubCommand};

fn main() -> Result<(), failure::Error> {
    let matches = App::new("mun")
        .version(env!("CARGO_PKG_VERSION"))
        .author("The Mun Project Developers")
        .about("The Mun executable enables compiling and running standalone Mun code")
        .setting(AppSettings::SubcommandRequired)
        .subcommand(
            SubCommand::with_name("build")
                .arg(
                    Arg::with_name("INPUT")
                        .help("Sets the input file to use")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::with_name("opt-level")
                        .short("O")
                        .help("optimize with possible levels 0-3"),
                )
                .about("Compiles a local Mun file into a module"),
        )
        .get_matches();

    let optimization_lvl = match matches.occurrences_of("opt-level") {
        0 => mun_compiler::OptimizationLevel::None,
        1 => mun_compiler::OptimizationLevel::Less,
        2 => mun_compiler::OptimizationLevel::Default,
        3 => mun_compiler::OptimizationLevel::Aggressive,
        _ => return Err(format_err!("Only optimization levels 0-3 are supported")),
    };

    match matches.subcommand() {
        ("build", Some(matches)) => build(matches, optimization_lvl)?,
        _ => unreachable!(),
    }

    Ok(())
}

/// Build the source file specified
fn build(
    matches: &ArgMatches,
    optimization_lvl: mun_compiler::OptimizationLevel,
) -> Result<(), failure::Error> {
    let options = mun_compiler::CompilerOptions {
        input: matches.value_of("INPUT").unwrap().into(), // Safe because its a required arg
        optimization_lvl,
    };
    mun_compiler::main(options)
}
