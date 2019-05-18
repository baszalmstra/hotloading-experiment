mod context;
mod diagnostic;
mod line_index;

use std::io;
use std::io::Write;

use diagnostic::Emit;

use mun_syntax::ast;
use mun_syntax::ast::AstNode;
use linefeed::{Interface, ReadResult};

use mun_codegen::code_gen;
use mun_errors::Diagnostic;

fn main() -> Result<(), failure::Error> {
    let term = Interface::new("mun")?;

    term.set_prompt("> ")?;

    let context = mun_codegen::inkwell::context::Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    println!("Welcome to the Mun CLI\nType :q and press enter to quit.");
    while let ReadResult::Input(input) = term.read_line()? {
        if !input.trim().is_empty() {
            term.add_history_unique(input.clone());
        }

        match input.as_str() {
            ":quit"|":q" => break,
            _ => {
                let line_index = line_index::LineIndex::new(&input);
                let source = ast::SourceFile::parse(&input);
                let errors = source.errors();
                if errors.len() > 0 {
                    // TODO: Improve errors
                    for err in errors {
                        Into::<Diagnostic>::into(err)
                            .emit(&line_index);
                    }
                } else {
                    //print!("{}", source.syntax().debug_dump());
                    let result = code_gen(&source, &module, &builder);
                    let errors = result.diagnostics;
                    if errors.len() > 0 {
                        for err in errors {
                            err.emit(&line_index);
                        }
                    } else {
                        for function in result.functions.values() {
                            println!("{}", function.print_to_string().to_string());
                        }
                    }
                }
            }
        };
    }

    Ok(())
}
