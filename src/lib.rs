use std::path::PathBuf;

use anyhow::Result;

use emulator::Emulator;

mod emulator;

#[derive(Debug, clap::Parser)]
#[command(author, version, about, long_about = None)]
pub struct Params {
    #[cfg(not(debug_assertions))]
    /// A path to the BIOS file
    bios: PathBuf,

    #[cfg(debug_assertions)]
    /// A path to the BIOS file
    #[arg(default_value = "roms/scph1001.bin")]
    bios: PathBuf,
}

pub fn run(params: Params) -> Result<()> {
    let mut emulator = Emulator::new(&params.bios)?;

    emulator.run()
}
