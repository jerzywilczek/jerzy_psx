use std::path::Path;

use anyhow::Result;

use cpu::Cpu;

mod cpu;
mod memory;

pub struct Emulator {
    cpu: Cpu,
}

impl Emulator {
    pub fn new(bios_path: &Path) -> Result<Self> {
        Ok(Self {
            cpu: Cpu::new(bios_path)?,
        })
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            self.cpu.cycle()?
        }
    }
}
