use clap::Parser;
use jerzy_psx::Params;

fn main() {
    let params = Params::parse();

    if let Err(e) = jerzy_psx::run(params) {
        let mut chain = e.chain().peekable();

        match chain.next() {
            Some(e) => println!("Error:\n    {}\n", e),
            None => println!("Unknown error"),
        }

        if chain.peek().is_some() {
            println!("Caused by:");

            for e in e.chain().skip(1) {
                println!("    {}", e);
            }
        }
    }
}
