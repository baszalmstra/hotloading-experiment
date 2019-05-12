mod context;
mod diagnostic;
mod line_index;

use std::io;
use std::io::Write;

use mun_syntax::ast;
use mun_syntax::ast::AstNode;

use diagnostic::Diagnostic;

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin.read_line(&mut input).expect("Failed to read line");
        let input = input.as_str().trim();
        if input == ".quit" {
            break;
        }

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
}
