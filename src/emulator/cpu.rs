use std::path::Path;

use anyhow::{bail, Context, Result};

use crate::emulator::memory::Memory;

pub struct Cpu {
    pc: u32,
    gp_regs: [u32; 32],
    memory: Memory,
}

impl Cpu {
    pub fn new(bios_path: &Path) -> Result<Self> {
        let mut gp_regs = [0xdeadbeef; 32];
        gp_regs[0] = 0;

        Ok(Self {
            pc: 0xbfc00000,
            gp_regs,
            memory: Memory::new(bios_path)?,
        })
    }

    fn load32(&mut self, addr: u32) -> Result<u32> {
        self.memory.load32(addr).context("CPU: load32 failed")
    }

    fn store32(&mut self, addr: u32, val: u32) -> Result<()> {
        self.memory.store32(addr, val)
    }

    fn reg(&self, reg: Register) -> u32 {
        self.gp_regs[reg.0 as usize]
    }

    fn set_reg(&mut self, reg: Register, val: u32) {
        self.gp_regs[reg.0 as usize] = val;

        self.gp_regs[0] = 0;
    }

    pub fn cycle(&mut self) -> Result<()> {
        let code = self.load32(self.pc)?;

        let insn = Instruction::decode(code)?;
        self.execute(insn)?;

        self.pc = self.pc.wrapping_add(4);

        Ok(())
    }

    fn execute(&mut self, insn: Instruction) -> Result<()> {
        #[cfg(feature = "cpu-debug")]
        println!("Executing instruction: {:?}", insn);

        match insn {
            Instruction::Lui { imm, rt } => self.set_reg(rt, imm << 16),
            Instruction::Ori { imm, rt, rs } => self.set_reg(rt, self.reg(rs) | imm),
            Instruction::Sw { rs, rt, imm } => {
                self.store32(self.reg(rs).wrapping_add(imm), self.reg(rt))?
            }
        }

        Ok(())
    }
}

#[derive(Clone, Copy)]
struct Register(u32);

impl std::fmt::Debug for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mnemonic = match self.0 {
            0 => "zero",
            1 => "at",
            2 => "v0",
            3 => "v1",
            4 => "a0",
            5 => "a1",
            6 => "a2",
            7 => "a3",
            8 => "t0",
            9 => "t1",
            10 => "t2",
            11 => "t3",
            12 => "t4",
            13 => "t5",
            14 => "t6",
            15 => "t7",
            16 => "s0",
            17 => "s1",
            18 => "s2",
            19 => "s3",
            20 => "s4",
            21 => "s5",
            22 => "s6",
            23 => "s7",
            24 => "t8",
            25 => "t9",
            26 => "k0",
            27 => "k1",
            28 => "gp",
            29 => "sp",
            30 => "fp/s8",
            31 => "ra",
            _ => "unknown",
        };

        f.debug_tuple("Register").field(&mnemonic).finish()
    }
}

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Lui {
        rt: Register,
        imm: u32,
    },

    Ori {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    Sw {
        rs: Register,
        rt: Register,
        imm: u32,
    },
}

impl Instruction {
    fn decode(code: u32) -> Result<Self> {
        match Self::opcode(code) {
            0x0d => Ok(Self::Ori {
                imm: Self::imm16(code),
                rt: Self::rt(code),
                rs: Self::rs(code),
            }),

            0x0f => Ok(Self::Lui {
                imm: Self::imm16(code),
                rt: Self::rt(code),
            }),

            0x2b => Ok(Self::Sw {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
            }),

            _ => bail!(
                "CPU: unable to decode instruction 0x{:08x} (opcode 0x{:02x})",
                code,
                Self::opcode(code)
            ),
        }
    }

    fn opcode(code: u32) -> u8 {
        (code >> 26) as u8
    }

    fn rt(code: u32) -> Register {
        Register((code >> 16) & 0x1f)
    }

    fn rs(code: u32) -> Register {
        Register((code >> 21) & 0x1f)
    }

    fn imm16(code: u32) -> u32 {
        code & 0xffff
    }

    fn imm16_se(code: u32) -> u32 {
        (code & 0xffff) as i16 as u32
    }
}
