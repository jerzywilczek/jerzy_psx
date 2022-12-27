use clap::Parser;
use jerzy_psx::Params;

fn main() {
    let params = Params::parse();

    jerzy_psx::run(params);
}
