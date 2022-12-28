use std::path::Path;

use anyhow::{bail, Context, Result};

use crate::emulator::memory::Memory;

pub struct Cpu {
    pc: u32,
    gp_regs: [u32; 32],
    memory: Memory,
    next_insn: Instruction,
}

impl Cpu {
    pub fn new(bios_path: &Path) -> Result<Self> {
        let mut gp_regs = [0xdeadbeef; 32];
        gp_regs[0] = 0;

        Ok(Self {
            pc: 0xbfc00000,
            gp_regs,
            memory: Memory::new(bios_path)?,
            next_insn: Instruction::decode(0).unwrap(), // noop
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

        let insn = self.next_insn;
        self.next_insn = Instruction::decode(code)?;

        self.pc = self.pc.wrapping_add(4);

        self.execute(insn)?;

        Ok(())
    }

    fn execute(&mut self, insn: Instruction) -> Result<()> {
        #[cfg(feature = "cpu-debug")]
        println!("Executing instruction: {:?}", insn);

        match insn {
            Instruction::Sll { rt, rd, imm } => self.set_reg(rd, self.reg(rt) << imm),
            Instruction::Or { rt, rs, rd } => self.set_reg(rd, self.reg(rs) | self.reg(rt)),
            Instruction::J { imm } => self.pc = (self.pc & 0xf0000000) | (imm << 2),
            Instruction::Addiu { rt, rs, imm } => self.set_reg(rt, self.reg(rs).wrapping_add(imm)),
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
    Sll {
        rt: Register,
        rd: Register,
        imm: u32,
    },

    Or {
        rt: Register,
        rs: Register,
        rd: Register,
    },

    J {
        imm: u32,
    },

    Addiu {
        rt: Register,
        rs: Register,
        imm: u32,
    },

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
            0x00 => match Self::secondary_opcode(code) {
                0x00 => Ok(Self::Sll { rt: Self::rt(code), rd: Self::rd(code), imm: Self::imm5(code) }),

                0x25 => Ok(Self::Or { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                _ => bail!(
                    "CPU: unable to decode instruction 0x{:08x} (opcode 0x{:02x}, secondary opcode: 0x{:02x})",
                    code,
                    Self::opcode(code),
                    Self::secondary_opcode(code),
                )
            }

            0x02 => Ok(Self::J { imm: Self::imm26(code) }),

            0x09 => Ok(Self::Addiu { rt: Self::rt(code), rs: Self::rs(code), imm: Self::imm16_se(code) }),

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
            )
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

    fn rd(code: u32) -> Register {
        Register((code >> 11) & 0x1f)
    }

    fn imm5(code: u32) -> u32 {
        (code >> 6) & 0x1f
    }

    fn imm16(code: u32) -> u32 {
        code & 0xffff
    }

    fn imm16_se(code: u32) -> u32 {
        (code & 0xffff) as i16 as u32
    }

    fn imm26(code: u32) -> u32 {
        code & 0x3ffffff
    }

    fn secondary_opcode(code: u32) -> u32 {
        code & 0x3f
    }
}
