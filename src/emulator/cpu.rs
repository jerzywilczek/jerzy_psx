use std::path::Path;

use anyhow::{bail, Context, Result};

use crate::emulator::memory::Memory;

use super::memory::Addressible;

pub struct Cpu {
    pc: u32,
    gp_regs: [u32; 32],
    memory: Memory,
    next_insn: Instruction,
    /// ## COP0 register 12: status register
    /// ### Bits we handle:
    ///  - 16 - Isc, Isolate cache: memory stores only target cache, not the real memory - not fully handled
    sr: u32,
    delayed_load: (Register, u32),
    current_load: (Register, u32),
    reg_to_set: (Register, u32),
    #[cfg(feature = "cpu-debug")]
    cycles: u32,
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
            sr: 0,
            delayed_load: (Register(0), 0),
            current_load: (Register(0), 0),
            reg_to_set: (Register(0), 0),
            #[cfg(feature = "cpu-debug")]
            cycles: 0,
        })
    }

    fn load<T: Addressible>(&mut self, addr: u32) -> Result<T> {
        self.memory
            .load(addr)
            .context(format!("CPU: load{} failed", T::WIDTH * 8))
    }

    fn store<T: Addressible>(&mut self, addr: u32, val: T) -> Result<()> {
        self.memory.store(addr, val)
    }

    fn reg(&self, reg: Register) -> u32 {
        self.gp_regs[reg.0 as usize]
    }

    fn set_reg(&mut self, reg: Register, val: u32) {
        self.reg_to_set = (reg, val);
    }

    fn set_reg_immediately(&mut self, reg: Register, val: u32) {
        self.gp_regs[reg.0 as usize] = val;

        self.gp_regs[0] = 0;
    }

    fn pop_scheduled_set_regs(&mut self) {
        for (reg, val) in [self.current_load, self.reg_to_set] {
            self.set_reg_immediately(reg, val);
        }

        for s in [&mut self.current_load, &mut self.reg_to_set] {
            *s = (Register(0), 0);
        }
    }

    pub fn cycle(&mut self) -> Result<()> {
        let code = self.load(self.pc)?;

        let insn = self.next_insn;
        self.next_insn = Instruction::decode(code)?;

        self.pc = self.pc.wrapping_add(4);

        self.current_load = self.delayed_load;
        self.delayed_load = (Register(0), 0);

        self.execute(insn)?;

        self.pop_scheduled_set_regs();

        #[cfg(feature = "cpu-debug")]
        {
            self.cycles += 1;
        }

        Ok(())
    }

    fn execute(&mut self, insn: Instruction) -> Result<()> {
        #[cfg(feature = "cpu-debug")]
        println!(
            "[cycle {:010}] Executing instruction: {:?}",
            self.cycles, insn
        );

        match insn {
            Instruction::Sll { rt, rd, imm } => self.set_reg(rd, self.reg(rt) << imm),

            Instruction::Jr { rs } => self.pc = self.reg(rs),

            Instruction::Jalr { rs, rd } => {
                self.set_reg(rd, self.pc);
                self.pc = self.reg(rs);
            }

            Instruction::Add { rs, rt, rd } => {
                let rs = self.reg(rs) as i32;
                let rt = self.reg(rt) as i32;

                let val = match rs.checked_add(rt) {
                    Some(v) => v as u32,
                    None => bail!("CPU: FIXME: ADD overflows are not handled"),
                };

                self.set_reg(rd, val)
            }

            Instruction::Addu { rs, rt, rd } => {
                self.set_reg(rd, self.reg(rs).wrapping_add(self.reg(rt)))
            }

            Instruction::And { rs, rt, rd } => self.set_reg(rd, self.reg(rs) & self.reg(rt)),

            Instruction::Or { rt, rs, rd } => self.set_reg(rd, self.reg(rs) | self.reg(rt)),

            Instruction::Sltu { rs, rt, rd } => {
                self.set_reg(rd, (self.reg(rs) < self.reg(rt)) as u32)
            }

            Instruction::Bcondz {
                rs,
                bcondz_specifier,
                imm,
            } => {
                let is_ge = bcondz_specifier & 1 != 0;
                let do_link = bcondz_specifier >> 1 == 8;

                let test = (self.reg(rs) as i32) < 0;
                let test = test ^ is_ge;

                if do_link {
                    self.set_reg(Register(31), self.pc);
                }

                if test {
                    self.branch(imm);
                }
            }

            Instruction::J { imm } => self.pc = (self.pc & 0xf0000000) | (imm << 2),

            Instruction::Jal { imm } => {
                self.set_reg(Register(31), self.pc);

                self.pc = (self.pc & 0xf0000000) | (imm << 2);
            }

            Instruction::Beq { rs, rt, imm } => {
                if self.reg(rs) == self.reg(rt) {
                    self.branch(imm);
                }
            }

            Instruction::Bne { rs, rt, imm } => {
                if self.reg(rs) != self.reg(rt) {
                    self.branch(imm);
                }
            }

            Instruction::Blez { rs, imm } => {
                if self.reg(rs) as i32 <= 0 {
                    self.branch(imm);
                }
            }

            Instruction::Bgtz { rs, imm } => {
                if self.reg(rs) as i32 > 0 {
                    self.branch(imm);
                }
            }

            Instruction::Addi { rs, rt, imm } => {
                let rs = self.reg(rs) as i32;

                let rs = match rs.checked_add(imm as i32) {
                    Some(rs) => rs as u32,
                    None => bail!("CPU: FIXME: ADDI overflow is not handled"),
                };

                self.set_reg(rt, rs);
            }

            Instruction::Addiu { rt, rs, imm } => self.set_reg(rt, self.reg(rs).wrapping_add(imm)),

            Instruction::Andi { rs, rt, imm } => self.set_reg(rt, self.reg(rs) & imm),

            Instruction::Ori { imm, rt, rs } => self.set_reg(rt, self.reg(rs) | imm),

            Instruction::Lui { imm, rt } => self.set_reg(rt, imm << 16),

            Instruction::Mfc0 { rt, rd } => match rd {
                CopRegister(12) => self.delayed_load = (rt, self.sr),

                CopRegister(r) => bail!("CPU: move from an unhandled COP0 register: {}", r),
            },

            Instruction::Mtc0 { rt, rd } => match rd {
                CopRegister(12) => self.sr = self.reg(rt),
                CopRegister(3) | CopRegister(5) | CopRegister(6) | CopRegister(7)
                | CopRegister(9) | CopRegister(11) => {
                    if self.reg(rt) != 0 {
                        bail!(
                            "CPU: {} moved to {rd:?}, which is a breakpoint register",
                            self.reg(rt)
                        )
                    }
                }

                CopRegister(13) => {
                    if self.reg(rt) != 0 {
                        bail!(
                            "CPU: {} moved to {rd:?}, which is the cause register",
                            self.reg(rt)
                        )
                    }
                }
                CopRegister(r) => bail!("CPU: move to an unhandled COP0 register: {r}"),
            },

            Instruction::Lb { rs, rt, imm } => {
                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do load
                    let val = self.load::<u8>(self.reg(rs).wrapping_add(imm))? as i8;

                    self.delayed_load = (rt, val as u32);
                }
            }

            Instruction::Lw { rs, rt, imm } => {
                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do load
                    let val = self.load(self.reg(rs).wrapping_add(imm))?;

                    self.delayed_load = (rt, val);
                }
            }

            Instruction::Lbu { rs, rt, imm } => {
                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do load
                    let val = self.load::<u8>(self.reg(rs).wrapping_add(imm))?;

                    self.delayed_load = (rt, val as u32);
                }
            }

            Instruction::Sb { rs, rt, imm } => {
                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do store
                    self.store(self.reg(rs).wrapping_add(imm), self.reg(rt) as u8)?
                }
            }

            Instruction::Sh { rs, rt, imm } => {
                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do store
                    self.store(self.reg(rs).wrapping_add(imm), self.reg(rt) as u16)?
                }
            }

            Instruction::Sw { rs, rt, imm } => {
                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do store
                    self.store(self.reg(rs).wrapping_add(imm), self.reg(rt))?
                }
            }
        }

        Ok(())
    }

    fn branch(&mut self, offset: u32) {
        let offset = offset << 2;

        self.pc = self.pc.wrapping_add(offset);
        self.pc = self.pc.wrapping_sub(4);
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

        f.write_fmt(format_args!("Register({})", mnemonic))
    }
}

#[derive(Debug, Clone, Copy)]
struct CopRegister(u32);

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Sll {
        rt: Register,
        rd: Register,
        imm: u32,
    },

    Jr {
        rs: Register,
    },

    Jalr {
        rs: Register,
        rd: Register,
    },

    Add {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    Addu {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    And {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    Or {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    Sltu {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    Bcondz {
        rs: Register,
        bcondz_specifier: u32,
        imm: u32,
    },

    J {
        imm: u32,
    },

    Jal {
        imm: u32,
    },

    Beq {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    Bne {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    Blez {
        rs: Register,
        imm: u32,
    },

    Bgtz {
        rs: Register,
        imm: u32,
    },

    Addi {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    Addiu {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    Andi {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    Ori {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    Lui {
        rt: Register,
        imm: u32,
    },

    Mfc0 {
        rt: Register,
        rd: CopRegister,
    },

    Mtc0 {
        rt: Register,
        rd: CopRegister,
    },

    Lb {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    Lw {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    Lbu {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    Sb {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    Sh {
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

                0x08 => Ok(Self::Jr { rs: Self::rs(code) }),

                0x09 => Ok(Self::Jalr { rs: Self::rs(code), rd: Self::rd(code) }),

                0x20 => Ok(Self::Add { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x21 => Ok(Self::Addu { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x24 => Ok(Self::And { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x25 => Ok(Self::Or { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x2b => Ok(Self::Sltu { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                _ => bail!(
                    "CPU: unable to decode instruction 0x{:08x} (opcode 0x{:02x}, secondary opcode: 0x{:02x})",
                    code,
                    Self::opcode(code),
                    Self::secondary_opcode(code),
                )
            }

            0x01 => Ok(Self::Bcondz { rs: Self::rs(code), bcondz_specifier: Self::bcondz_specifier(code), imm: Self::imm16_se(code) }),

            0x02 => Ok(Self::J { imm: Self::imm26(code) }),

            0x03 => Ok(Self::Jal { imm: Self::imm26(code) }),

            0x04 => Ok(Self::Beq { rs: Self::rs(code), rt: Self::rt(code), imm: Self::imm16_se(code) }),

            0x05 => Ok(Self::Bne { rs: Self::rs(code), rt: Self::rt(code), imm: Self::imm16_se(code) }),

            0x06 => Ok(Self::Blez { rs: Self::rs(code), imm: Self::imm16_se(code) }),

            0x07 => Ok(Self::Bgtz { rs: Self::rs(code), imm: Self::imm16_se(code) }),

            0x08 => Ok(Self::Addi { rs: Self::rs(code), rt: Self::rt(code), imm: Self::imm16_se(code) }),

            0x09 => Ok(Self::Addiu { rt: Self::rt(code), rs: Self::rs(code), imm: Self::imm16_se(code) }),

            0x0c => Ok(Self::Andi { rs: Self::rs(code), rt: Self::rt(code), imm: Self::imm16(code) }),

            0x0d => Ok(Self::Ori {
                imm: Self::imm16(code),
                rt: Self::rt(code),
                rs: Self::rs(code),
            }),

            0x0f => Ok(Self::Lui {
                imm: Self::imm16(code),
                rt: Self::rt(code),
            }),

            0x10 => match Self::cop_opcode(code) {
                0x00 => Ok(Self::Mfc0 { rt: Self::rt(code), rd: Self::rd_cop(code) }),

                0x04 => Ok(Self::Mtc0 { rt: Self::rt(code), rd: Self::rd_cop(code) }),
                _ => bail!(
                    "CPU: unable to decode coprocessor instruction 0x{:08x} (opcode 0x{:02x}, coprocessor opcode: 0x{:02x})",
                    code,
                    Self::opcode(code),
                    Self::cop_opcode(code)
                )
            }

            0x20 => Ok(Self::Lb {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
            }),

            0x23 => Ok(Self::Lw {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
            }),

            0x24 => Ok(Self::Lbu {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
            }),

            0x28 => Ok(Self::Sb {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
            }),

            0x29 => Ok(Self::Sh {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
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

    fn rs(code: u32) -> Register {
        Register((code >> 21) & 0x1f)
    }

    fn rt(code: u32) -> Register {
        Register((code >> 16) & 0x1f)
    }

    fn _rt_cop(code: u32) -> CopRegister {
        CopRegister((code >> 16) & 0x1f)
    }

    fn rd(code: u32) -> Register {
        Register((code >> 11) & 0x1f)
    }

    fn rd_cop(code: u32) -> CopRegister {
        CopRegister((code >> 11) & 0x1f)
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

    fn cop_opcode(code: u32) -> u32 {
        (code >> 21) & 0x1f
    }

    fn bcondz_specifier(code: u32) -> u32 {
        (code >> 16) & 0x1f
    }
}
