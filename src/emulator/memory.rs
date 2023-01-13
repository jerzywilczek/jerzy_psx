use std::{fmt::Display, ops::Range, path::Path};

use anyhow::{bail, Context, Result};

pub struct Memory {
    bios: Bios,
    ram: Ram,
}

pub trait Addressible: Copy {
    const WIDTH: u32;

    fn from_u32(val: u32) -> Self;
    fn from_le_bytes(bytes: &[u8]) -> Self;
    fn to_u32(self) -> u32;
    fn write_for_mem_ctl_1_ok(self, offset: u32) -> bool;
}

impl Addressible for u8 {
    const WIDTH: u32 = 1;

    fn from_u32(val: u32) -> Self {
        val as u8
    }

    fn from_le_bytes(bytes: &[u8]) -> Self {
        let bytes: [u8; Self::WIDTH as usize] = bytes.try_into().unwrap();

        Self::from_le_bytes(bytes)
    }

    fn to_u32(self) -> u32 {
        self as u32
    }

    fn write_for_mem_ctl_1_ok(self, _: u32) -> bool {
        false
    }
}

impl Addressible for u16 {
    const WIDTH: u32 = 2;

    fn from_u32(val: u32) -> Self {
        val as u16
    }

    fn from_le_bytes(bytes: &[u8]) -> Self {
        let bytes: [u8; Self::WIDTH as usize] = bytes.try_into().unwrap();

        Self::from_le_bytes(bytes)
    }

    fn to_u32(self) -> u32 {
        self as u32
    }

    fn write_for_mem_ctl_1_ok(self, _: u32) -> bool {
        false
    }
}

impl Addressible for u32 {
    const WIDTH: u32 = 4;

    fn from_u32(val: u32) -> Self {
        val
    }

    fn from_le_bytes(bytes: &[u8]) -> Self {
        let bytes: [u8; Self::WIDTH as usize] = bytes.try_into().unwrap();

        Self::from_le_bytes(bytes)
    }

    fn to_u32(self) -> u32 {
        self
    }

    fn write_for_mem_ctl_1_ok(self, offset: u32) -> bool {
        if offset == 0 && self != 0x1f000000 {
            return false;
        }

        if offset == 4 && self != 0x1f802000 {
            return false;
        }

        true
    }
}

impl Memory {
    pub fn new(bios_path: &Path) -> Result<Self> {
        let bios =
            Bios::new(bios_path).context("Memory: error while creating the BIOS component")?;

        let ram = Ram::new();

        Ok(Self { bios, ram })
    }

    pub fn load<T: Addressible>(&self, addr: u32) -> Result<T> {
        if addr % T::WIDTH != 0 {
            bail!("Memory: attempted a load{} from a non-aligned address (0x{addr:08x} % {} = {} != 0)", T::WIDTH * 8, T::WIDTH, addr % 4);
        }

        let addr = from_masked(addr);

        if let Some(offset) = offset_in(addr, map::RAM) {
            return Ok(self.ram.load(offset));
        }

        if offset_in(addr, map::EXPANSION_1).is_some() {
            // FIXME: expansions are not implemented
            return Ok(T::from_u32(!0));
        }

        if let Some(offset) = offset_in(addr, map::BIOS) {
            return Ok(self.bios.load(offset));
        }

        if let Some(_offset) = offset_in(addr, map::MEM_CTL_1) {
            bail!("Memory: FIXME: load from mem ctl 1")
        }

        if offset_in(addr, map::MEM_CTL_2).is_some() {
            bail!("Memory: FIXME: load from mem ctl 2")
        }

        if offset_in(addr, map::DMA).is_some() {
            println!("Memory: FIXME: load from DMA");
            return Ok(T::from_u32(0));
        }

        if offset_in(addr, map::INTERRUPT_CTL).is_some() {
            println!("Memory: FIXME: read from interrupt ctl always return 0");
            return Ok(T::from_u32(0));
        }

        if offset_in(addr, map::SPU).is_some() {
            println!("Memory: FIXME: load from the SPU");
            return Ok(T::from_u32(0));
        }

        if offset_in(addr, map::MEM_CTL_3).is_some() {
            bail!("Memory: FIXME: load from mem ctl 3")
        }

        bail!(
            "Memory: attempted a load from a non-mapped address: {}",
            addr
        );
    }

    pub fn store<T: Addressible>(&mut self, addr: u32, val: T) -> Result<()> {
        if addr % T::WIDTH != 0 {
            bail!(
                "Memory: attempted a store{} to a non-aligned address (0x{addr:08x} % {} = {} != 0)",
                T::WIDTH * 8,
                T::WIDTH,
                addr % T::WIDTH
            );
        }

        let addr = from_masked(addr);

        if let Some(offset) = offset_in(addr, map::RAM) {
            self.ram.store(offset, val);
            return Ok(());
        }

        if offset_in(addr, map::BIOS).is_some() {
            bail!("Memory: attempted a write to BIOS memory: {}", addr)
        }

        if let Some(offset) = offset_in(addr, map::MEM_CTL_1) {
            if val.write_for_mem_ctl_1_ok(offset) {
                println!("FIXME: Unhandled mem ctl 1 write (addr = {})", addr);
                return Ok(());
            } else {
                bail!("Memory: writing a bad value to one of the expansion addresses")
            }
        }

        if offset_in(addr, map::MEM_CTL_2).is_some() {
            // FIXME: Does this need to be ignored?
            return Ok(());
        }

        if offset_in(addr, map::DMA).is_some() {
            println!("Memory: FiXME: DMA store ({:#x} to {})", val.to_u32(), addr);
            return Ok(());
        }

        if offset_in(addr, map::INTERRUPT_CTL).is_some() {
            println!("Memory: FIXME: store to interrupt ctl memory");
            return Ok(());
        }

        if offset_in(addr, map::TIMERS).is_some() {
            if val.to_u32() != 0 {
                bail!("Memory: Store to the timers mmio");
            }

            return Ok(());
        }

        if offset_in(addr, map::SPU).is_some() {
            println!("Memory: FIXME: store to spu memory");
            return Ok(());
        }

        if offset_in(addr, map::EXPANSION_2).is_some() {
            // store to exp 2 can probably be ignored
            return Ok(());
        }

        if let Some(_offset) = offset_in(addr, map::MEM_CTL_3) {
            println!("Memory: FIXME: store to mem ctl 3 (cache control)");
            return Ok(());
        }

        bail!(
            "Memory: attempted a store to a non-mapped address: {}",
            addr
        );
    }
}

fn offset_in(addr: PhysAddr, range: Range<u32>) -> Option<u32> {
    if !range.contains(&addr.0) {
        return None;
    }

    Some(addr.0 - range.start)
}

#[derive(Debug, Clone, Copy)]
struct PhysAddr(u32);

impl Display for PhysAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("0x{:08x}P", self.0))
    }
}

fn from_masked(addr: u32) -> PhysAddr {
    #[rustfmt::skip]
    const REGION_MASK: [u32; 8] = [
        // KUSEG - 2048 MiB
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
        // KSEG0 - 512 MiB
        0x7fffffff,
        // KSEG1 - 512 MiB
        0x1fffffff,
        // KSEG2 - 1024 MiB
        0xffffffff, 0xffffffff
    ];

    let i = (addr >> 29) as usize;

    PhysAddr(addr & REGION_MASK[i])
}

mod map {
    use std::ops::Range;

    pub const RAM_SIZE: u32 = 2 * 1024 * 1024;
    pub const RAM: Range<u32> = 0x00000000..RAM_SIZE;

    pub const EXPANSION_1_SIZE: u32 = 8 * 1024 * 1024;
    pub const EXPANSION_1: Range<u32> = 0x1f000000..0x1f000000 + EXPANSION_1_SIZE;

    pub const BIOS_SIZE: u32 = 512 * 1024;
    pub const BIOS: Range<u32> = 0x1fc00000..0x1fc00000 + BIOS_SIZE;

    pub const MEM_CTL_1_SIZE: u32 = 36;
    /// Memory and device frequencies and such things
    pub const MEM_CTL_1: Range<u32> = 0x1f801000..0x1f801000 + MEM_CTL_1_SIZE;

    pub const MEM_CTL_2_SIZE: u32 = 4;
    /// Memory size, perhaps
    pub const MEM_CTL_2: Range<u32> = 0x1f801060..0x1f801060 + MEM_CTL_2_SIZE;

    pub const DMA_SIZE: u32 = 0x80;
    /// Direct Memory Access registers
    pub const DMA: Range<u32> = 0x1f801080..0x1f801080 + DMA_SIZE;

    pub const INTERRUPT_CTL_SIZE: u32 = 8;
    /// Interrupt control
    pub const INTERRUPT_CTL: Range<u32> = 0x1f801070..0x1f801074 + INTERRUPT_CTL_SIZE;

    pub const TIMERS_SIZE: u32 = 0x32;
    /// Timers
    pub const TIMERS: Range<u32> = 0x1f801100..0x1f801100 + TIMERS_SIZE;

    pub const SPU_SIZE: u32 = 1024;
    /// Sound Processing Unit
    pub const SPU: Range<u32> = 0x1f801c00..0x1f801c00 + SPU_SIZE;

    pub const EXPANSION_2_SIZE: u32 = 128;
    /// Expansion 2 region, used for some debugging I think
    pub const EXPANSION_2: Range<u32> = 0x1f802000..0x1f802000 + EXPANSION_2_SIZE;

    pub const MEM_CTL_3_SIZE: u32 = 4;
    /// Cache control
    pub const MEM_CTL_3: Range<u32> = 0xfffe0130..0xfffe0130 + MEM_CTL_3_SIZE;
}

struct Ram {
    data: Vec<u8>,
}

impl Ram {
    fn new() -> Self {
        Self {
            data: vec![0xfa; 2 * 1024 * 1024],
        }
    }

    fn load<T: Addressible>(&self, offset: u32) -> T {
        let offset = offset as usize;

        T::from_le_bytes(&self.data[offset..offset + T::WIDTH as usize])
    }

    fn store<T: Addressible>(&mut self, offset: u32, val: T) {
        let offset = offset as usize;
        // let a = &mut self.data[offset..offset + T::WIDTH as usize].try_into().unwrap();
        let bytes = &val.to_u32().to_le_bytes()[..T::WIDTH as usize];

        self.data[offset..offset + T::WIDTH as usize].copy_from_slice(bytes);
    }
}

struct Bios {
    data: Vec<u8>,
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

    fn load<T: Addressible>(&self, offset: u32) -> T {
        let offset = offset as usize;

        T::from_le_bytes(&self.data[offset..offset + T::WIDTH as usize])
    }
}
