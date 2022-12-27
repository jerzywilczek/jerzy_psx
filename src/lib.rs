use std::path::PathBuf;

#[derive(Debug, clap::Parser)]
#[command(author, version, about, long_about = None)]
pub struct Params {
    /// A path to the BIOS file
    bios: PathBuf,
}

pub fn run(params: Params) {
    println!("Works! {:?}", params.bios);
}
