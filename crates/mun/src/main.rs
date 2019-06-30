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
                .about("Compiles a local Mun file into a module"),
        )
        .get_matches();

    match matches.subcommand() {
        ("build", Some(matches)) => build(matches)?,
        _ => unreachable!(),
    }

    Ok(())
}

/// Build the source file specified
fn build(matches: &ArgMatches) -> Result<(), failure::Error> {
    let options = mun_compiler::CompilerOptions {
        input: matches.value_of("INPUT").unwrap().into(), // Safe because its a required arg
    };
    mun_compiler::main(options)
}
