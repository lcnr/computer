use std::mem;

pub const RAM_SIZE: usize = 1 << 16;

#[derive(Default, Debug, Clone, Copy)]
pub struct Registers {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
}

#[derive(Clone)]
pub struct Remu {
    ram: Box<[u8; RAM_SIZE]>,
    mem: [u8; 2],
    pc: [u8; 2],
    reg: Registers,
    expected: Vec<u8>,
}

impl Default for Remu {
    fn default() -> Self {
        Self {
            ram: Box::new([0; RAM_SIZE]),
            mem: [0, 0],
            pc: [0, 0],
            reg: Default::default(),
            expected: Vec::new(),
        }
    }
}

impl Remu {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn memory(&self) -> &[u8; RAM_SIZE] {
        &self.ram
    }

    pub fn memory_mut(&mut self) -> &mut [u8; RAM_SIZE] {
        &mut self.ram
    }

    pub fn registers(&self) -> Registers {
        self.reg
    }

    pub fn set_registers(&mut self, reg: Registers) {
        self.reg = reg;
    }

    pub fn program_counter(&self) -> u16 {
        u16::from_le_bytes(self.pc)
    }

    pub fn set_program_counter(&mut self, pc: u16) {
        self.pc = pc.to_le_bytes();
    }

    pub fn memory_pointer(&self) -> u16 {
        u16::from_le_bytes(self.mem)
    }

    pub fn set_memory_pointer(&mut self, mem: u16) {
        self.mem = mem.to_le_bytes();
    }

    pub fn pointee(&self) -> u8 {
        self.ram[self.memory_pointer() as usize]
    }

    pub fn pointee_mut(&mut self) -> &mut u8 {
        &mut self.ram[self.memory_pointer() as usize]
    }

    pub fn set_pointee(&mut self, value: u8) {
        self.ram[self.memory_pointer() as usize] = value
    }

    fn read(&mut self, command_offset: u8) -> u8 {
        match command_offset {
            0 => self.reg.a,
            1 => self.reg.b,
            2 => self.reg.c,
            3 => self.reg.d,
            4 => self.pointee(),
            5 => {
                let v = self.ram[self.program_counter() as usize];
                self.pc[0] = self.pc[0].wrapping_add(1);
                v
            }
            e => unreachable!(e),
        }
    }

    fn write(&mut self, command_offset: u8) -> &mut u8 {
        match command_offset {
            0 => &mut self.reg.a,
            1 => &mut self.reg.b,
            2 => &mut self.reg.c,
            3 => &mut self.reg.d,
            4 => self.pointee_mut(),
            5 => &mut self.mem[0],
            6 => &mut self.mem[1],
            e => unreachable!(e),
        }
    }

    fn check(&mut self) -> Result<(), StepError> {
        if let Some(expected) = self.expected.pop() {
            if self.reg.a == expected {
                Ok(())
            } else {
                Err(StepError::Check {
                    expected,
                    found: self.pointee(),
                })
            }
        } else {
            Err(StepError::NoExpected)
        }
    }

    fn inner_step(&mut self, in_cond: bool) -> Result<bool, StepError> {
        let command = self.ram[self.program_counter() as usize];
        self.pc[0] = self.pc[0].wrapping_add(1);

        match command {
            0x00 => (),
            0x01..=0x07 => {
                *self.write(command - 0x01) = self.reg.a.checked_add(self.reg.b).unwrap()
            }
            0x08..=0x0e => {
                *self.write(command - 0x08) = self.reg.a.checked_sub(self.reg.b).unwrap()
            }
            0x0f..=0x15 => *self.write(command - 0x0f) = self.reg.a & self.reg.b,
            0x16..=0x1c => *self.write(command - 0x16) = self.reg.a | self.reg.b,
            0x1d..=0x23 => *self.write(command - 0x1d) = self.reg.a ^ self.reg.b,
            0x24..=0x2a => *self.write(command - 0x24) = !self.reg.a,
            0x2b..=0x31 => {
                *self.write(command - 0x2b) = self.reg.a.checked_shr(self.reg.b as u32).unwrap_or(0)
            }
            0x32..=0x38 => {
                *self.write(command - 0x32) = self.reg.a.checked_shl(self.reg.b as u32).unwrap_or(0)
            }
            0x39..=0x5c => {
                let mut w = (command - 0x39) % 6;
                let r = (command - 0x39) / 6;
                if r <= w + 1 {
                    w += 1;
                }

                *self.write(w) = self.read(r);
            }
            0x5d => mem::swap(&mut self.reg.a, &mut self.reg.b),
            0x60..=0x65 => self.pc[0] = self.read(command - 0x60),
            0x66..=0x6b => {
                self.pc[1] = self.read(command - 0x66);
                self.pc[0] = 0;
            }
            0x6c..=0x89 => {
                let mut s = (command - 0x6c) % 5;
                let b = (command - 0x6c) / 5;

                if b < s + 1 {
                    s += 1;
                }

                let s = self.read(s);
                let b = self.read(b);
                self.pc[1] = b;
                self.pc[0] = s;
            }
            0xc0..=0xcf => {
                if in_cond {
                    return Err(StepError::DoubleBranch);
                }

                let offset = command - 0xc0;
                let cmd_offset = offset % 8;
                let cond = match cmd_offset {
                    0 => self.reg.a == 0,
                    1 => self.reg.a != 0,
                    2 => self.reg.a > self.reg.b,
                    3 => self.reg.a >= self.reg.b,
                    4 => self.reg.a == self.reg.b,
                    5 => self.reg.a != self.reg.b,
                    6 => self.reg.a <= self.reg.b,
                    7 => self.reg.a < self.reg.b,
                    _ => unreachable!(),
                };

                if cond {
                    return self.inner_step(true);
                } else {
                    if cmd_offset == offset {
                        self.pc[0] = self.pc[0].wrapping_add(1);
                    } else {
                        self.pc[0] = self.pc[0].wrapping_add(2);
                    }
                }
            }
            0xf0 => {
                self.expected
                    .push(self.ram[self.program_counter() as usize]);
                self.pc[0] = self.pc[0].wrapping_add(1);
            }
            0xf1 => self.check()?,
            0xf2 => eprintln!(
                "[0x{:04x}]: {v:03} 0x{v:02x} 0b{v:08b}",
                self.program_counter(),
                v = self.reg.a
            ),
            0xff => {
                self.pc[0] = self.pc[0].wrapping_sub(1);
                return Ok(false);
            }
            0x5e | 0x5f | 0x8a..=0xbf | 0xd0..=0xef | 0xf3..=0xfe => {
                return Err(StepError::UnknownCommand(command))
            }
        }

        Ok(true)
    }

    /// Executes one step, returning false in case the program would halt.
    pub fn step(&mut self) -> Result<bool, StepError> {
        self.inner_step(false)
    }

    pub fn run(&mut self, max_steps: usize) -> Result<usize, RunError> {
        for i in 0..max_steps {
            if !self.step()? {
                if self.expected.is_empty() {
                    return Ok(i);
                } else {
                    return Err(RunError::MissingChecks);
                }
            }
        }

        Err(RunError::MaxSteps)
    }
}

#[derive(Debug, Clone)]
pub enum StepError {
    NoExpected,
    UnknownCommand(u8),
    DoubleBranch,
    Check { expected: u8, found: u8 },
}

impl From<StepError> for RunError {
    fn from(e: StepError) -> Self {
        RunError::StepError(e)
    }
}

#[derive(Debug, Clone)]
pub enum RunError {
    MaxSteps,
    MissingChecks,
    StepError(StepError),
}
