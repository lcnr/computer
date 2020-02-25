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
}

impl Default for Remu {
    fn default() -> Self {
        Self {
            ram: Box::new([0; RAM_SIZE]),
            mem: [0, 0],
            pc: [0, 0],
            reg: Default::default(),
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

    pub fn set_pointee(&mut self, value: u8) {
        self.ram[self.memory_pointer() as usize] = value
    }

    pub fn step(&mut self) {
        let command = self.ram[self.program_counter() as usize];
        self.pc[0] = self.pc[0].wrapping_add(1);

        match command {
            0x00 => (),
            0x01 => self.reg.a = self.reg.a.wrapping_add(self.reg.b),
            0x02 => self.reg.b = self.reg.a.wrapping_add(self.reg.b),
            0x03 => self.reg.c = self.reg.a.wrapping_add(self.reg.b),
            0x04 => self.reg.d = self.reg.a.wrapping_add(self.reg.b),
            0x05 => self.set_pointee(self.reg.a.wrapping_add(self.reg.b)),
            0x06 => self.mem[0] = self.reg.a.wrapping_add(self.reg.b),
            0x07 => self.mem[1] = self.reg.a.wrapping_add(self.reg.b),
            _ => unimplemented!(),
        }
    }
}
