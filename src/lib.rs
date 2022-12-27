use std::path::PathBuf;

use anyhow::Result;

use emulator::Emulator;

mod emulator;

#[derive(Debug, clap::Parser)]
#[command(author, version, about, long_about = None)]
pub struct Params {
    /// A path to the BIOS file
    bios: PathBuf,
}

pub fn run(params: Params) -> Result<()> {
    let mut emulator = Emulator::new(&params.bios)?;

    emulator.run()
}
