use std::{fmt::Display, ops::Range, path::Path};

use anyhow::{bail, Context, Result};

use dma::Dma;

pub struct Memory {
    bios: Bios,
    dma: Dma,
    ram: Ram,
}

pub trait Addressible: Copy {
    const WIDTH: u32;

    fn from_u32(val: u32) -> Self;
    fn from_le_bytes(bytes: &[u8]) -> Self;
    fn to_u32(self) -> u32;
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
}

impl Memory {
    pub fn new(bios_path: &Path) -> Result<Self> {
        let bios =
            Bios::new(bios_path).context("Memory: error while creating the BIOS component")?;

        let dma = Dma::new();

        let ram = Ram::new();

        Ok(Self { bios, dma, ram })
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

        if let Some(offset) = offset_in(addr, map::DMA) {
            if T::WIDTH != 4 {
                bail!("Memory: FIXME: figure out what to do with DMA halfword and byte access")
            }
            return self.dma.reg(offset).map(|val| T::from_u32(val));
        }

        if offset_in(addr, map::INTERRUPT_CTL).is_some() {
            println!("Memory: FIXME: read from interrupt ctl always return 0");
            return Ok(T::from_u32(0));
        }

        if let Some(offset) = offset_in(addr, map::GPU) {
            println!("Memory: FIXME: load from a GPU register");

            let val = match offset {
                // FIXME: This is just so that the BIOS thinks the GPU can receive another DMA block
                4 => 0x10000000,
                _ => 0,
            };

            return Ok(T::from_u32(val));
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
            match offset {
                0 => {
                    if val.to_u32() == 0x1f000000 {
                        return Ok(());
                    } else {
                        bail!("Memory: writing a bad value to one of the expansion addresses")
                    }
                }

                4 => {
                    if val.to_u32() == 0x1f802000 {
                        return Ok(());
                    } else {
                        bail!("Memory: writing a bad value to one of the expansion addresses")
                    }
                }

                _ => {
                    println!("Memory: FIXME: unhandled write to MEM CTL 1");
                    return Ok(());
                }
            }
        }

        if offset_in(addr, map::MEM_CTL_2).is_some() {
            // FIXME: Does this need to be ignored?
            return Ok(());
        }

        if let Some(offset) = offset_in(addr, map::DMA) {
            if T::WIDTH != 4 {
                bail!("Memory: FIXME: figure out how to handle halfword- and byte-long stores ")
            }

            return self.dma.set_reg(offset, val.to_u32());
        }

        if offset_in(addr, map::INTERRUPT_CTL).is_some() {
            println!("Memory: FIXME: store to interrupt ctl memory");
            return Ok(());
        }

        if offset_in(addr, map::TIMERS).is_some() {
            println!("Memory: FIXME: Store to the timers mmio");

            return Ok(());
        }

        if offset_in(addr, map::GPU).is_some() {
            println!("Memory: FIXME: store to a GPU register");

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

    pub const GPU_SIZE: u32 = 8;
    /// GPU
    pub const GPU: Range<u32> = 0x1f801810..0x1f801810 + GPU_SIZE;

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

mod dma {
    use anyhow::{bail, Result};
    pub struct Dma {
        control: u32,
        interrupt: InterruptReg,
        channels: [Channel; 7],
    }

    impl Dma {
        pub fn new() -> Self {
            Self {
                control: 0x07654321,
                interrupt: InterruptReg::new(),
                channels: Default::default(),
            }
        }

        fn port_and_port_offset(offset: u32) -> (u32, u32) {
            ((offset & 0x70) >> 4, offset & 0xf)
        }

        pub fn reg(&self, offset: u32) -> Result<u32> {
            fn error(offset: u32) -> Result<u32> {
                bail!("DMA: unhandled register access (offset = {:#x})", offset)
            }

            let (port, port_offset) = Self::port_and_port_offset(offset);

            let val = match port {
                0..=6 => {
                    let channel = self.channel(port.try_into().unwrap());

                    match port_offset {
                        0 => channel.base_addr.reg(),

                        4 => channel.block_ctl.reg(),

                        8 => channel.ctl.reg(),

                        _ => return error(offset),
                    }
                }

                7 => match port_offset {
                    0 => self.control,

                    4 => self.interrupt.reg(),

                    _ => return error(offset),
                },

                _ => return error(offset),
            };

            #[cfg(feature = "dma-debug")]
            println!("DMA register read: [{:#x}] = {:#x}", offset, val);

            Ok(val)
        }

        pub fn set_reg(&mut self, offset: u32, val: u32) -> Result<()> {
            fn error(offset: u32, val: u32) -> Result<()> {
                bail!(
                    "DMA: unhandled register store (offset = {:#x}, value: 0x{:08x})",
                    offset,
                    val
                )
            }

            #[cfg(feature = "dma-debug")]
            println!("DMA register set: {:#x} => {:#x}", val, offset);

            let (port, port_offset) = Self::port_and_port_offset(offset);

            match port {
                0..=6 => {
                    let channel = self.channel_mut(port.try_into().unwrap());

                    match port_offset {
                        0 => channel.base_addr.set(val),

                        4 => channel.block_ctl.set(val),

                        8 => channel.ctl.set(val),

                        _ => return error(offset, val),
                    }
                }

                7 => match port_offset {
                    0 => self.set_control(val),

                    4 => self.interrupt.set(val),

                    _ => return error(offset, val),
                },

                _ => return error(offset, val),
            }

            Ok(())
        }

        fn channel(&self, port: Port) -> &Channel {
            &self.channels[port as usize]
        }

        fn channel_mut(&mut self, port: Port) -> &mut Channel {
            &mut self.channels[port as usize]
        }

        fn set_control(&mut self, val: u32) {
            self.control = val;
        }
    }

    struct InterruptReg {
        /// The use of the low 5 bits if the interrupt register is unknown, but they are RW
        low5: u8,
        force_irq: bool,
        irq_enable: u8,
        irq_enable_signal: bool,
        irq_flags: u8,
    }

    impl InterruptReg {
        fn new() -> Self {
            Self {
                low5: 0,
                force_irq: false,
                irq_enable: 0,
                irq_enable_signal: false,
                irq_flags: 0,
            }
        }

        fn irq_signal(&self) -> bool {
            self.force_irq || (self.irq_enable_signal && self.irq_flags > 0)
        }

        fn reg(&self) -> u32 {
            self.low5 as u32
                | (self.force_irq as u32) << 15
                | (self.irq_enable as u32) << 16
                | (self.irq_enable_signal as u32) << 23
                | (self.irq_flags as u32) << 24
                | (self.irq_signal() as u32) << 31
        }

        fn set(&mut self, val: u32) {
            self.low5 = (val & 0x1f) as u8;
            self.force_irq = val & (1 << 15) != 0;
            self.irq_enable = ((val >> 16) & 0x7f) as u8;
            self.irq_enable_signal = val & (1 << 23) != 0;
            // writing a 1 to a flag acknowledges it
            self.irq_flags &= !((val >> 24) & 0x7f) as u8;
        }
    }

    /// DMA transfer direction
    #[derive(Debug, Default, Clone, Copy)]
    #[repr(u8)]
    enum Direction {
        #[default]
        ToRam = 0,
        FromRam,
    }

    impl TryFrom<u8> for Direction {
        type Error = anyhow::Error;

        fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
            match value {
                0 => Ok(Self::ToRam),
                1 => Ok(Self::FromRam),
                _ => bail!("Tried to construct Direction from a bad value"),
            }
        }
    }

    /// DMA transfer step
    #[derive(Debug, Default, Clone, Copy)]
    #[repr(u8)]
    enum TransferStep {
        #[default]
        Increment,
        Decrement,
    }

    impl TryFrom<u8> for TransferStep {
        type Error = anyhow::Error;

        fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
            match value {
                0 => Ok(Self::Increment),
                1 => Ok(Self::Decrement),
                _ => bail!("Tried to construct TransferStep from a bad value"),
            }
        }
    }

    /// DMA sync mode
    #[derive(Debug, Default, Clone, Copy)]
    #[repr(u8)]
    enum SyncMode {
        #[default]
        Manual = 0,
        Block,
        LinkedList,
    }

    impl TryFrom<u8> for SyncMode {
        type Error = anyhow::Error;

        fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
            match value {
                0 => Ok(Self::Manual),
                1 => Ok(Self::Block),
                2 => Ok(Self::LinkedList),
                _ => bail!("Tried to construct SyncMode from a bad value"),
            }
        }
    }

    #[derive(Debug, Default)]
    struct Channel {
        ctl: ChannelCtl,
        base_addr: BaseAddr,
        block_ctl: BlockCtl,
    }

    /// Block control register
    #[derive(Debug, Default)]
    struct BlockCtl {
        block_size: u16,
        block_amount: u16,
    }

    impl BlockCtl {
        fn reg(&self) -> u32 {
            ((self.block_amount as u32) << 16) | (self.block_size as u32)
        }

        fn set(&mut self, val: u32) {
            self.block_amount = (val >> 16) as u16;
            self.block_size = (val & 0xffff) as u16
        }
    }

    /// Base address register
    #[derive(Debug, Default)]
    struct BaseAddr(u32);

    impl BaseAddr {
        fn reg(&self) -> u32 {
            self.0
        }

        fn set(&mut self, addr: u32) {
            self.0 = addr & 0x00ffffff
        }
    }

    /// DMA channel control register
    #[derive(Debug, Default)]
    struct ChannelCtl {
        transfer_direction: Direction,
        memory_addr_step: TransferStep,
        enable_chopping: bool,
        sync_mode: SyncMode,
        chopping_dma_window_size: u8,
        chopping_cpu_window_size: u8,
        start_busy: bool,
        start_trigger: bool,
        dummy: u8,
    }

    impl ChannelCtl {
        fn reg(&self) -> u32 {
            self.transfer_direction as u32
                | (self.memory_addr_step as u32) << 1
                | (self.enable_chopping as u32) << 8
                | (self.sync_mode as u32) << 9
                | (self.chopping_dma_window_size as u32) << 16
                | (self.chopping_cpu_window_size as u32) << 20
                | (self.start_busy as u32) << 24
                | (self.start_trigger as u32) << 28
                | (self.dummy as u32) << 29
        }

        fn set(&mut self, val: u32) {
            self.transfer_direction = (val as u8 & 1).try_into().unwrap();
            self.memory_addr_step = ((val >> 1) as u8 & 1).try_into().unwrap();
            self.enable_chopping = (val >> 8) & 1 != 0;
            self.sync_mode = ((val >> 9) as u8 & 3).try_into().unwrap();
            self.chopping_dma_window_size = (val >> 16) as u8 & 7;
            self.chopping_cpu_window_size = (val >> 20) as u8 & 7;
            self.start_busy = (val >> 24) & 1 != 0;
            self.start_trigger = (val >> 28) & 1 != 0;
            self.dummy = (val >> 29) as u8 & 3;
        }
    }

    /// The devices that use the DMA
    #[derive(Debug, Clone, Copy)]
    enum Port {
        /// Macroblock decoder in
        MdecIn = 0,
        /// Macroblock decoder out
        MdecOut,
        /// GPU
        Gpu,
        /// CD-ROM
        Cdrom,
        /// Sound processing unit
        Spu,
        /// Extension port
        Pio,
        /// Clear the ordering table
        Otc,
    }

    impl TryFrom<u32> for Port {
        type Error = anyhow::Error;

        fn try_from(value: u32) -> std::result::Result<Self, Self::Error> {
            match value {
                0 => Ok(Self::MdecIn),
                1 => Ok(Self::MdecOut),
                2 => Ok(Self::Gpu),
                3 => Ok(Self::Cdrom),
                4 => Ok(Self::Spu),
                5 => Ok(Self::Pio),
                6 => Ok(Self::Otc),
                n => bail!("Tried to construct a DMA Port with a bad number ({})", n),
            }
        }
    }
}
