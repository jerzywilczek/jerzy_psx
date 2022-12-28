use std::{ops::Range, path::Path};

use anyhow::{bail, Context, Result};

pub struct Memory {
    bios: Bios,
}

impl Memory {
    pub fn new(bios_path: &Path) -> Result<Self> {
        let bios =
            Bios::new(bios_path).context("Memory: error while creating the BIOS component")?;

        Ok(Self { bios })
    }

    pub fn load32(&self, addr: u32) -> Result<u32> {
        if addr % 4 != 0 {
            bail!("Memory: attempted a load32 from a non-aligned address (0x{addr:08x} % 4 = {} != 0)", addr % 4);
        }

        if let Some(offset) = offset_in(addr, map::BIOS) {
            return Ok(self.bios.load32(offset));
        }

        if let Some(_offset) = offset_in(addr, map::MEM_CTL_1) {
            bail!("Memory: FIXME: loads from mem ctl 1")
        }

        if offset_in(addr, map::MEM_CTL_2).is_some() {
            bail!("Memory: load32 from mem ctl 2")
        }

        if offset_in(addr, map::MEM_CTL_3).is_some() {
            bail!("Memory: load32 from mem ctl 3")
        }

        bail!(
            "Memory: attempted a load32 from a non-mapped address: 0x{:08x}",
            addr
        );
    }

    pub fn store32(&mut self, addr: u32, val: u32) -> Result<()> {
        if addr % 4 != 0 {
            bail!(
                "Memory: attempted a store32 to a non-aligned address (0x{addr:08x} % 4 = {} != 0)",
                addr % 4
            );
        }

        if offset_in(addr, map::BIOS).is_some() {
            bail!("Memory: attempted a write to BIOS memory: 0x{:08x}", addr)
        }

        if let Some(offset) = offset_in(addr, map::MEM_CTL_1) {
            match offset {
                0 if val != 0x1f000000 => bail!("Memory: writing a value different than 0x1f000000 to expansion 1 base address not permitted"),
                4 if val != 0x1f802000 => bail!("Memory: writing a value different than 0x1f802000 to expansion 2 base address not permitted"),
                _ => {
                    println!("FIXME: Unhandled mem ctl 1 write (addr = 0x{:08x})", addr);
                    return Ok(());
                }
            }
        }

        if offset_in(addr, map::MEM_CTL_2).is_some() {
            // FIXME: Does this need to be ignored?
            return Ok(());
        }

        if let Some(_offset) = offset_in(addr, map::MEM_CTL_3) {
            println!("Memory: FIXME: store to mem ctl 3 (cache control)");
            return Ok(());
        }

        bail!(
            "Memory: attempted a store32 to a non-mapped address: 0x{:08x}",
            addr
        );
    }
}

fn offset_in(addr: u32, range: Range<u32>) -> Option<u32> {
    if !range.contains(&addr) {
        return None;
    }

    Some(addr - range.start)
}

struct Bios {
    data: Vec<u8>,
}

mod map {
    use std::ops::Range;

    pub const BIOS_SIZE: u32 = 512 * 1024;
    pub const BIOS: Range<u32> = 0xbfc00000..0xbfc00000 + BIOS_SIZE;

    pub const MEM_CTL_1_SIZE: u32 = 36;
    /// Memory and device frequencies and such things
    pub const MEM_CTL_1: Range<u32> = 0x1f801000..0x1f801000 + MEM_CTL_1_SIZE;

    pub const MEM_CTL_2_SIZE: u32 = 4;
    /// Memory size, perhaps
    pub const MEM_CTL_2: Range<u32> = 0x1f801060..0x1f801060 + MEM_CTL_2_SIZE;

    pub const MEM_CTL_3_SIZE: u32 = 4;
    /// Cache control
    pub const MEM_CTL_3: Range<u32> = 0xfffe0130..0xfffe0130 + MEM_CTL_3_SIZE;
}

impl Bios {
    fn new(path: &Path) -> Result<Self> {
        use std::fs;

        let data = fs::read(path).context("BIOS: error while reading the BIOS file from disk")?;

        if data.len() as u32 != map::BIOS_SIZE {
            bail!(
                "BIOS: file read from {:?}, size expected to be {}, turned out to be {}",
                path,
                map::BIOS_SIZE,
                data.len()
            );
        }

        Ok(Self { data })
    }

    fn load32(&self, offset: u32) -> u32 {
        let offset = offset as usize;

        if let [b0, b1, b2, b3] = self.data[offset..offset + 4] {
            let b0 = b0 as u32;
            let b1 = b1 as u32;
            let b2 = b2 as u32;
            let b3 = b3 as u32;

            return (b3 << 24) | (b2 << 16) | (b1 << 8) | b0;
        }

        panic!("BIOS: attempted a read from a address outside of BIOS memory range")
    }
}
