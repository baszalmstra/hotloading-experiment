mod context;
mod diagnostic;
mod line_index;

use std::io;
use std::io::Write;

use mun_syntax::ast;
use mun_syntax::ast::AstNode;
use linefeed::{Interface, ReadResult};

use diagnostic::Diagnostic;

fn main() -> Result<(), failure::Error> {
    let term = Interface::new("mun")?;

    term.set_prompt("> ")?;

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
                        let diagnostic = Diagnostic::new(&line_index, err);
                        println!("{}", diagnostic);
                    }
                } else {
                    print!("{}", source.syntax().debug_dump());
                }
            }
        };
    }

    Ok(())
}
