use std::path::Path;

use anyhow::{bail, Context, Result};

use crate::emulator::memory::Memory;

use super::memory::Addressible;

pub struct Cpu {
    /// Program Counter.
    /// During Instruction execution, this points at the instruction that will be executed in the next cycle.
    pc: u32,

    /// Address of the currently executed instruction, used for setting the EPC reg on exceptions
    cur_insn_addr: u32,

    /// This becomes the new pc during the cycle.
    /// When an instruction is executed, this points to the instruction after the next unexecuted instruction.
    next_pc: u32,

    /// This gets set when some kind of a jump occurs
    jumped: bool,

    /// Whether the currently executed instruction is in a delay slot
    delay_slot: bool,

    /// General purpose registers
    gp_regs: [u32; 32],
    memory: Memory,

    /// ## COP0 register 12: status register
    /// ### Bits we handle:
    ///  - 16 - Isc, Isolate cache: memory stores only target cache, not the real memory - not fully handled
    ///  - 22 - BEV, Boot Exception Vectors in RAM/ROM - controls whether the exception handler is at 0xbfc00180 or 0x80000080
    sr: u32,

    /// ## COP0 register 13: cause register
    cause: u32,

    ///## COP0 register 14: EPC - Exception Program Counter
    epc: u32,

    /// HI register, used for multiplication high result and division remainder
    hi: u32,

    /// LO register, used for multiplication low result and division quotient
    lo: u32,
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

        let pc = 0xbfc00000;

        Ok(Self {
            pc,
            cur_insn_addr: 0,
            next_pc: pc.wrapping_add(4),
            jumped: false,
            delay_slot: false,
            gp_regs,
            memory: Memory::new(bios_path)?,
            sr: 0,
            cause: 0,
            epc: 0xdeadbeef,
            hi: 0xdeadbeef,
            lo: 0xdeadbeef,
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
        self.cur_insn_addr = self.pc;

        if self.pc % 4 != 0 {
            self.raise(Exception::LoadAddressError);
            return Ok(());
        }

        let code = self.load(self.pc)?;

        self.pc = self.next_pc;
        self.next_pc = self.pc.wrapping_add(4);

        self.current_load = self.delayed_load;
        self.delayed_load = (Register(0), 0);

        self.delay_slot = self.jumped;
        self.jumped = false;

        self.execute(Instruction::decode(code)?)?;

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

            Instruction::Srl { rt, rd, imm } => self.set_reg(rd, self.reg(rt) >> imm),

            Instruction::Sra { rt, rd, imm } => {
                self.set_reg(rd, (self.reg(rt) as i32 >> imm) as u32)
            }

            Instruction::Sllv { rs, rt, rd } => {
                self.set_reg(rd, self.reg(rt) << (self.reg(rs) & 0x1f))
            }

            Instruction::Srlv { rs, rt, rd } => {
                self.set_reg(rd, self.reg(rt) >> (self.reg(rs) & 0x1f))
            }

            Instruction::Srav { rs, rt, rd } => {
                self.set_reg(rd, ((self.reg(rt) as i32) >> (self.reg(rs) & 0x1f)) as u32)
            }

            Instruction::Jr { rs } => {
                self.jump(self.reg(rs));
            }

            Instruction::Jalr { rs, rd } => {
                self.set_reg(rd, self.next_pc);
                self.jump(self.reg(rs));
            }

            Instruction::Syscall => self.raise(Exception::Syscall),

            Instruction::Break => self.raise(Exception::Break),

            Instruction::Mfhi { rd } => self.set_reg(rd, self.hi),

            Instruction::Mthi { rs } => self.hi = self.reg(rs),

            Instruction::Mflo { rd } => self.set_reg(rd, self.lo),

            Instruction::Mtlo { rs } => self.lo = self.reg(rs),

            Instruction::Mult { rs, rt } => {
                let a = (self.reg(rs) as i32) as i64;
                let b = (self.reg(rt) as i32) as i64;

                // Fixme: this should take more time than a single cycle...

                let result = (a * b) as u64;

                self.hi = (result >> 32) as u32;
                self.lo = result as u32;
            }

            Instruction::Multu { rs, rt } => {
                let a = self.reg(rs) as u64;
                let b = self.reg(rt) as u64;

                // Fixme: this should take more time than a single cycle...

                let result = a * b;

                self.hi = (result >> 32) as u32;
                self.lo = result as u32;
            }

            Instruction::Div { rs, rt } => {
                let n = self.reg(rs) as i32;
                let d = self.reg(rt) as i32;

                // Fixme: this should take more time than a single cycle...

                if d == 0 {
                    // div by zero special case
                    self.hi = n as u32;
                    self.lo = if n >= 0 { 0xffffffff } else { 1 }
                } else if n as u32 == 0x80000000 && d == -1 {
                    // result does not fit
                    self.hi = 0;
                    self.lo = 0x80000000;
                } else {
                    self.hi = (n % d) as u32;
                    self.lo = (n / d) as u32;
                }
            }

            Instruction::Divu { rs, rt } => {
                let n = self.reg(rs);
                let d = self.reg(rt);

                // Fixme: this should take more time than a single cycle...

                if d == 0 {
                    // div by zero special case
                    self.hi = n;
                    self.lo = 0xffffffff;
                } else {
                    self.hi = n % d;
                    self.lo = n / d;
                }
            }

            Instruction::Add { rs, rt, rd } => {
                let rs = self.reg(rs) as i32;
                let rt = self.reg(rt) as i32;

                let val = match rs.checked_add(rt) {
                    Some(v) => v as u32,
                    None => {
                        self.raise(Exception::Overflow);
                        return Ok(());
                    }
                };

                self.set_reg(rd, val)
            }

            Instruction::Addu { rs, rt, rd } => {
                self.set_reg(rd, self.reg(rs).wrapping_add(self.reg(rt)))
            }

            Instruction::Sub { rs, rt, rd } => {
                let rs = self.reg(rs) as i32;
                let rt = self.reg(rt) as i32;

                let val = match rs.checked_sub(rt) {
                    Some(v) => v as u32,
                    None => {
                        self.raise(Exception::Overflow);
                        return Ok(());
                    }
                };

                self.set_reg(rd, val)
            }

            Instruction::Subu { rs, rt, rd } => {
                self.set_reg(rd, self.reg(rs).wrapping_sub(self.reg(rt)))
            }

            Instruction::And { rs, rt, rd } => self.set_reg(rd, self.reg(rs) & self.reg(rt)),

            Instruction::Or { rt, rs, rd } => self.set_reg(rd, self.reg(rs) | self.reg(rt)),

            Instruction::Xor { rs, rt, rd } => self.set_reg(rd, self.reg(rs) ^ self.reg(rt)),

            Instruction::Nor { rs, rt, rd } => self.set_reg(rd, !(self.reg(rs) | self.reg(rt))),

            Instruction::Slt { rs, rt, rd } => {
                self.set_reg(rd, ((self.reg(rs) as i32) < self.reg(rt) as i32) as u32)
            }

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
                    self.set_reg(Register(31), self.next_pc);
                }

                if test {
                    self.branch(imm);
                }
            }

            Instruction::J { imm } => self.jump((self.next_pc & 0xf0000000) | (imm << 2)),

            Instruction::Jal { imm } => {
                self.set_reg(Register(31), self.next_pc);

                self.jump((self.next_pc & 0xf0000000) | (imm << 2));
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

            Instruction::Slti { rs, rt, imm } => {
                self.set_reg(rt, ((self.reg(rs) as i32) < imm as i32) as u32)
            }

            Instruction::Sltiu { rs, rt, imm } => self.set_reg(rt, (self.reg(rs) < imm) as u32),

            Instruction::Andi { rs, rt, imm } => self.set_reg(rt, self.reg(rs) & imm),

            Instruction::Ori { imm, rt, rs } => self.set_reg(rt, self.reg(rs) | imm),

            Instruction::Xori { imm, rt, rs } => self.set_reg(rt, self.reg(rs) ^ imm),

            Instruction::Lui { imm, rt } => self.set_reg(rt, imm << 16),

            Instruction::Mfc0 { rt, rd } => {
                let val = match rd {
                    CopRegister(12) => self.sr,
                    CopRegister(13) => self.cause,
                    CopRegister(14) => self.epc,

                    CopRegister(r) => bail!("CPU: move from an unhandled COP0 register: {}", r),
                };

                self.delayed_load = (rt, val);
            }

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

            Instruction::Cop1 => self.raise(Exception::CoprocessorError),

            Instruction::Cop3 => self.raise(Exception::CoprocessorError),

            Instruction::Rfe => {
                let mode = self.sr & 0x3f;
                self.sr &= !0xf;
                self.sr |= mode >> 2;
            }

            Instruction::Lb { rs, rt, imm } => {
                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do load
                    let val = self.load::<u8>(self.reg(rs).wrapping_add(imm))? as i8;

                    self.delayed_load = (rt, val as u32);
                }
            }

            Instruction::Lh { rs, rt, imm } => {
                let addr = self.reg(rs).wrapping_add(imm);

                if addr % 2 != 0 {
                    self.raise(Exception::LoadAddressError);
                    return Ok(());
                }

                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do load
                    let val = self.load::<u16>(addr)? as i16;

                    self.delayed_load = (rt, val as u32);
                }
            }

            Instruction::Lwl { rs, rt, imm } => {
                let addr = self.reg(rs).wrapping_add(imm);

                let val = if self.current_load.0 == rt {
                    self.current_load.1
                } else {
                    0
                };

                let aligned_word: u32 = self.load(addr & !3)?;

                let val = match addr & 3 {
                    0 => (val & 0x00ffffff) | (aligned_word << 24),
                    1 => (val & 0x0000ffff) | (aligned_word << 16),
                    2 => (val & 0x000000ff) | (aligned_word << 8),
                    3 => aligned_word,
                    _ => unreachable!(),
                };

                self.delayed_load = (rt, val);
            }

            Instruction::Lw { rs, rt, imm } => {
                let addr = self.reg(rs).wrapping_add(imm);

                if addr % 4 != 0 {
                    self.raise(Exception::LoadAddressError);
                    return Ok(());
                }

                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do load
                    let val = self.load(addr)?;

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

            Instruction::Lhu { rs, rt, imm } => {
                let addr = self.reg(rs).wrapping_add(imm);

                if addr % 2 != 0 {
                    self.raise(Exception::LoadAddressError);
                    return Ok(());
                }

                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do load
                    let val = self.load::<u8>(addr)?;

                    self.delayed_load = (rt, val as u32);
                }
            }

            Instruction::Lwr { rs, rt, imm } => {
                let addr = self.reg(rs).wrapping_add(imm);

                let val = if self.current_load.0 == rt {
                    self.current_load.1
                } else {
                    0
                };

                let aligned_word: u32 = self.load(addr & !3)?;

                let val = match addr & 3 {
                    0 => aligned_word,
                    1 => (val & 0xff000000) | (aligned_word >> 8),
                    2 => (val & 0xffff0000) | (aligned_word >> 16),
                    3 => (val & 0xffffff00) | (aligned_word >> 24),
                    _ => unreachable!(),
                };

                self.delayed_load = (rt, val);
            }

            Instruction::Sb { rs, rt, imm } => {
                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do store
                    self.store(self.reg(rs).wrapping_add(imm), self.reg(rt) as u8)?
                }
            }

            Instruction::Sh { rs, rt, imm } => {
                let addr = self.reg(rs).wrapping_add(imm);

                if addr % 2 != 0 {
                    self.raise(Exception::StoreAddressError);
                    return Ok(());
                }

                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do store
                    self.store(addr, self.reg(rt) as u16)?
                }
            }

            Instruction::Swl { rs, rt, imm } => {
                let addr = self.reg(rs).wrapping_add(imm);
                let aligned_addr = addr & !3;
                let aligned_mem: u32 = self.load(aligned_addr)?;
                let val = self.reg(rt);

                let val = match addr & 3 {
                    0 => (aligned_mem & 0xffffff00) | (val >> 24),
                    1 => (aligned_mem & 0xffff0000) | (val >> 24),
                    2 => (aligned_mem & 0xff000000) | (val >> 24),
                    3 => val,
                    _ => unreachable!(),
                };

                self.store(aligned_addr, val)?;
            }

            Instruction::Sw { rs, rt, imm } => {
                let addr = self.reg(rs).wrapping_add(imm);

                if addr % 4 != 0 {
                    self.raise(Exception::StoreAddressError);
                    return Ok(());
                }

                if self.sr & 0x10000 == 0 {
                    // cache is not isolated, do store
                    self.store(addr, self.reg(rt))?
                }
            }

            Instruction::Swr { rs, rt, imm } => {
                let addr = self.reg(rs).wrapping_add(imm);
                let aligned_addr = addr & !3;
                let aligned_mem: u32 = self.load(aligned_addr)?;
                let val = self.reg(rt);

                let val = match addr & 3 {
                    0 => val,
                    1 => (aligned_mem & 0x000000ff) | (val << 8),
                    2 => (aligned_mem & 0x0000ffff) | (val << 16),
                    3 => (aligned_mem & 0x00ffffff) | (val << 24),
                    _ => unreachable!(),
                };

                self.store(aligned_addr, val)?;
            }

            Instruction::Lwc0 => self.raise(Exception::CoprocessorError),

            Instruction::Lwc1 => self.raise(Exception::CoprocessorError),

            Instruction::Lwc2 { .. } => bail!("CPU: FIXME: Load to coprocessor 2"),

            Instruction::Lwc3 => self.raise(Exception::CoprocessorError),

            Instruction::Swc0 => self.raise(Exception::CoprocessorError),

            Instruction::Swc1 => self.raise(Exception::CoprocessorError),

            Instruction::Swc2 { .. } => bail!("CPU: FIXME: Store from coprocessor 2"),

            Instruction::Swc3 => self.raise(Exception::CoprocessorError),

            Instruction::Illegal { code } => {
                println!("CPU: Illegal instruction encountered: 0x{:08x}", code);
                self.raise(Exception::IllegalInstruction);
            }
        }

        Ok(())
    }

    fn branch(&mut self, offset: u32) {
        let offset = offset << 2;
        self.jump(self.next_pc.wrapping_add(offset).wrapping_sub(4))
    }

    fn jump(&mut self, addr: u32) {
        self.next_pc = addr;
        self.jumped = true;
    }

    fn raise(&mut self, exception: Exception) {
        let handler = if self.sr & (1 << 22) != 0 {
            0xbfc00180
        } else {
            0x80000080
        };

        let mode_stack = self.sr & 0x3f;
        self.sr &= !0x3f;
        self.sr |= (mode_stack << 2) & 0x3f;

        self.cause = (exception as u32) << 2;

        self.epc = self.cur_insn_addr;

        if self.delay_slot {
            self.epc = self.epc.wrapping_sub(4);
            self.cause |= 1 << 31;
        }

        self.pc = handler;
        self.next_pc = self.pc.wrapping_add(4);
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum Exception {
    LoadAddressError = 0x04,
    StoreAddressError = 0x05,
    Syscall = 0x08,
    Break = 0x09,
    IllegalInstruction = 0x0a,
    CoprocessorError = 0x0b,
    Overflow = 0x0c,
}

#[derive(Clone, Copy, PartialEq, Eq)]
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
    /// Shift Left Logical
    Sll {
        rt: Register,
        rd: Register,
        imm: u32,
    },

    /// Shift Right Logical
    Srl {
        rt: Register,
        rd: Register,
        imm: u32,
    },

    /// Shift Right Arithmetical
    Sra {
        rt: Register,
        rd: Register,
        imm: u32,
    },

    /// Shift Loft Logical Variable
    Sllv {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// Shift Right Logical Variable
    Srlv {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// Shift Right Arithmetical Variable
    Srav {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// Jump to Register
    Jr { rs: Register },

    /// Jump to Register And Link
    Jalr { rs: Register, rd: Register },

    /// SYSCALL
    Syscall,

    /// BREAK
    Break,

    /// Move From HI
    Mfhi { rd: Register },

    /// Move To HI
    Mthi { rs: Register },

    /// Move From LO
    Mflo { rd: Register },

    /// Move To LO
    Mtlo { rs: Register },

    /// MULTiply (signed)
    Mult { rs: Register, rt: Register },

    /// MULTiply Unsigned
    Multu { rs: Register, rt: Register },

    /// DIVide (signed)
    Div { rs: Register, rt: Register },

    /// DIVide Unsigned
    Divu { rs: Register, rt: Register },

    /// ADD (with signed overflow trap)
    Add {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// ADD Unsigned (w/o signed overflow trap)
    Addu {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// SUBtract (with signed overflow trap)
    Sub {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// SUBtract Unsigned (w/o signed overflow trap)
    Subu {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// logical AND
    And {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// bitwise OR
    Or {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// bitwise XOR
    Xor {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// bitwise NOR
    Nor {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// Set on Less Than (signed)
    Slt {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// Set on Less Than Unsigned
    Sltu {
        rs: Register,
        rt: Register,
        rd: Register,
    },

    /// One of:
    ///  - BLTZ - Branch on Less Than Zero
    ///  - BGEZ - Branch on Greater or Equal to Zero
    ///  - BLTZAL - Branch on Less Than Zero And Link
    ///  - BGEZAL - Branch on Greater or Equal to Zero And Link
    ///
    /// The `bcondz_specifier` specifies which one it is
    Bcondz {
        rs: Register,
        bcondz_specifier: u32,
        imm: u32,
    },

    /// Jump (to immediate)
    J { imm: u32 },

    /// Jump And Link (to immediate)
    Jal { imm: u32 },

    /// Branch on EQual
    Beq {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Branch on Not Equal
    Bne {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Branch on Less or Equal to Zero
    Blez { rs: Register, imm: u32 },

    /// Branch on Greater Than Zero
    Bgtz { rs: Register, imm: u32 },

    /// ADD Immediate (with signed overflow trap)
    Addi {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// ADD Immediate Unsigned (w/o signed overflow trap)
    Addiu {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Set on Less Than Immediate (signed)
    Slti {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Set on Less Than Immediate Unsigned
    Sltiu {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// AND Immediate
    Andi {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// OR Immediate
    Ori {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// XOR Immediate
    Xori {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Load Upper Immediate
    Lui { rt: Register, imm: u32 },

    /// Move From Cop0
    Mfc0 { rt: Register, rd: CopRegister },

    /// Move To Cop0
    Mtc0 { rt: Register, rd: CopRegister },

    /// COProcessor 1 instruction
    Cop1,

    /// COProcessor 3 instruction
    Cop3,

    /// Return From Exception
    Rfe,

    /// Load Byte (signed)
    Lb {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Load Halfword (signed)
    Lh {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Load Word Left
    Lwl {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Load Word
    Lw {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Load Byte Unsigned
    Lbu {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Load Halfword Unsigned
    Lhu {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Load Word Right
    Lwr {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Store Byte
    Sb {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Store Halfword
    Sh {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Store Word Left
    Swl {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Store Word
    Sw {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Store Word Right
    Swr {
        rs: Register,
        rt: Register,
        imm: u32,
    },

    /// Load Word to Coprocessor 0
    Lwc0,

    /// Load Word to Coprocessor 1
    Lwc1,

    /// Load Word to Coprocessor 2
    Lwc2 {
        _rs: Register,
        _rt: CopRegister,
        _imm: u32,
    },

    /// Load Word to Coprocessor 3
    Lwc3,

    /// Store Word from Coprocessor 0
    Swc0,

    /// Store Word from Coprocessor 1
    Swc1,

    /// Store Word from Coprocessor 2
    Swc2 {
        _rs: Register,
        _rt: CopRegister,
        _imm: u32,
    },

    /// Store Word from Coprocessor 3
    Swc3,

    /// an illegal instruction
    Illegal { code: u32 },
}

impl Instruction {
    fn decode(code: u32) -> Result<Self> {
        match Self::opcode(code) {
            0x00 => match Self::secondary_opcode(code) {
                0x00 => Ok(Self::Sll { rt: Self::rt(code), rd: Self::rd(code), imm: Self::imm5(code) }),

                0x02 => Ok(Self::Srl { rt: Self::rt(code), rd: Self::rd(code), imm: Self::imm5(code) }),

                0x03 => Ok(Self::Sra { rt: Self::rt(code), rd: Self::rd(code), imm: Self::imm5(code) }),

                0x04 => Ok(Self::Sllv { rs: Self::rs(code), rt: Self::rt(code), rd: Self::rd(code) }),

                0x06 => Ok(Self::Srlv { rs: Self::rs(code), rt: Self::rt(code), rd: Self::rd(code) }),

                0x07 => Ok(Self::Srav { rs: Self::rs(code), rt: Self::rt(code), rd: Self::rd(code) }),

                0x08 => Ok(Self::Jr { rs: Self::rs(code) }),

                0x09 => Ok(Self::Jalr { rs: Self::rs(code), rd: Self::rd(code) }),

                0x0c => Ok(Self::Syscall),

                0x0d => Ok(Self::Break),

                0x10 => Ok(Self::Mfhi { rd: Self::rd(code) }),

                0x11 => Ok(Self::Mthi { rs: Self::rs(code) }),

                0x12 => Ok(Self::Mflo { rd: Self::rd(code) }),

                0x13 => Ok(Self::Mtlo { rs: Self::rs(code) }),

                0x18 => Ok(Self::Mult { rs: Self::rs(code), rt: Self::rt(code) }),

                0x19 => Ok(Self::Multu { rs: Self::rs(code), rt: Self::rt(code) }),

                0x1a => Ok(Self::Div { rs: Self::rs(code), rt: Self::rt(code) }),

                0x1b => Ok(Self::Divu { rs: Self::rs(code), rt: Self::rt(code) }),

                0x20 => Ok(Self::Add { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x21 => Ok(Self::Addu { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x22 => Ok(Self::Sub { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x23 => Ok(Self::Subu { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x24 => Ok(Self::And { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x25 => Ok(Self::Or { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x26 => Ok(Self::Xor { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x27 => Ok(Self::Nor { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x2a => Ok(Self::Slt { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                0x2b => Ok(Self::Sltu { rt: Self::rt(code), rs: Self::rs(code), rd: Self::rd(code) }),

                _ => Ok(Self::Illegal { code })
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

            0x0a => Ok(Self::Slti { rs: Self::rs(code), rt: Self::rt(code), imm: Self::imm16_se(code) }),

            0x0b => Ok(Self::Sltiu { rs: Self::rs(code), rt: Self::rt(code), imm: Self::imm16_se(code) }),

            0x0c => Ok(Self::Andi { rs: Self::rs(code), rt: Self::rt(code), imm: Self::imm16(code) }),

            0x0d => Ok(Self::Ori { rs: Self::rs(code), rt: Self::rt(code), imm: Self::imm16(code) }),

            0x0e => Ok(Self::Xori { rs: Self::rs(code), rt: Self::rt(code), imm: Self::imm16(code) }),

            0x0f => Ok(Self::Lui {
                imm: Self::imm16(code),
                rt: Self::rt(code),
            }),

            0x10 => match Self::cop_opcode(code) {
                0x00 => Ok(Self::Mfc0 { rt: Self::rt(code), rd: Self::rd_cop(code) }),

                0x04 => Ok(Self::Mtc0 { rt: Self::rt(code), rd: Self::rd_cop(code) }),

                0x10 => match Self::secondary_opcode(code) {
                    0x10 => Ok(Self::Rfe),

                    _ => bail!("CPU: Illegal coprocessor instruction: 0x{:08x}", code),
                }

                _ => bail!(
                    "CPU: unable to decode coprocessor instruction 0x{:08x} (opcode 0x{:02x}, coprocessor opcode: 0x{:02x})",
                    code,
                    Self::opcode(code),
                    Self::cop_opcode(code)
                )
            }

            0x11 => Ok(Self::Cop1),

            0x12 => bail!(
                "CPU: FIXME: encountered GTE instruction 0x{:08x} (opcode 0x{:02x}, coprocessor opcode: 0x{:02x}). The GTE is not implemented",
                code,
                Self::opcode(code),
                Self::cop_opcode(code)
            ),

            0x13 => Ok(Self::Cop3),

            0x20 => Ok(Self::Lb {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
            }),

            0x21 => Ok(Self::Lh {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
            }),

            0x22 => Ok(Self::Lwl {
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

            0x25 => Ok(Self::Lhu {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
            }),

            0x26 => Ok(Self::Lwr {
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

            0x2a => Ok(Self::Swl {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
            }),

            0x2b => Ok(Self::Sw {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
            }),

            0x2e => Ok(Self::Swr {
                rs: Self::rs(code),
                rt: Self::rt(code),
                imm: Self::imm16_se(code),
            }),

            0x30 => Ok(Self::Lwc0),

            0x31 => Ok(Self::Lwc1),

            0x32 => Ok(Self::Lwc2 {
                _rs: Self::rs(code),
                _rt: Self::rt_cop(code),
                _imm: Self::imm16_se(code), // FIXME: I don't know if this should be sign-extended
            }),

            0x33 => Ok(Self::Lwc3),

            0x38 => Ok(Self::Swc0),

            0x39 => Ok(Self::Swc1),

            0x3a => Ok(Self::Swc2 {
                _rs: Self::rs(code),
                _rt: Self::rt_cop(code),
                _imm: Self::imm16_se(code), // FIXME: I don't know if this should be sign-extended
            }),

            0x3b => Ok(Self::Swc3),

            _ => Ok(Self::Illegal { code }),
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

    fn rt_cop(code: u32) -> CopRegister {
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
