use clap::{App, SubCommand};

use tools::{
    Result, generate, Overwrite
};

fn main() -> Result<()> {
    let matches = App::new("tasks")
        .setting(clap::AppSettings::SubcommandRequiredElseHelp)
        .subcommand(SubCommand::with_name("gen-syntax"))
        .get_matches();
    match matches.subcommand_name().expect("Subcommand must be specified") {
        "gen-syntax" => generate(Overwrite)?,
        _ => unreachable!(),
    }
    Ok(())
}
