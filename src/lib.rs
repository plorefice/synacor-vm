//! Rust implementation of the architecture specification from the
//! [Synacor Challenge](https://challenge.synacor.com/).
#![warn(missing_docs)]

/// A virtual machine which is able to load and execute programs built for this architecture.
#[derive(Debug, Clone)]
pub struct VirtualMachine {
    registers: [u16; 8],
    memory: [u16; 32768],
    stack: Vec<u16>,

    ip: usize, // instruction pointer
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self {
            registers: Default::default(),
            memory: [0; 32768],
            stack: Default::default(),

            ip: 0,
        }
    }
}

impl VirtualMachine {
    /// Creates a new, fresh virtual machine instance, with no loaded program.
    pub fn new() -> Self {
        Default::default()
    }

    /// Loads and run a program, returning in case an halt instruction is encountered or in case
    /// an error occurs.
    pub fn run(&mut self, program: &[u8]) {
        self.load(program);

        loop {
            if self.step().is_err() {
                return;
            }
        }
    }

    /// Loads the program file into memory, starting address 0.
    fn load(&mut self, program: &[u8]) {
        let mut words = program.chunks_exact(2).enumerate();

        while let Some((i, &[lo, hi])) = words.next() {
            self.memory[i] = u16::from(hi) << 8 | u16::from(lo);
        }
    }

    /// Performs a single step of the VM, fetching, decoding and executing the next instruction.
    fn step(&mut self) -> Result<(), ()> {
        match self.fetch() {
            Instruction::Halt => return Err(()),
            Instruction::Push(op) => {
                self.stack.push(self.value(op));
            }
            Instruction::Pop(reg) => {
                self.registers[reg] = self.stack.pop().unwrap();
            }
            Instruction::Set(reg, value) => {
                self.registers[reg] = self.value(value);
            }
            Instruction::Eq(reg, a, b) => {
                self.registers[reg] = if self.value(a) == self.value(b) { 1 } else { 0 };
            }
            Instruction::GreaterThan(reg, a, b) => {
                self.registers[reg] = if self.value(a) > self.value(b) { 1 } else { 0 };
            }
            Instruction::Jump(dest) => {
                self.ip = usize::from(self.value(dest));
            }
            Instruction::JumpTrue(cond, dest) => {
                if self.value(cond) != 0 {
                    self.ip = usize::from(self.value(dest));
                }
            }
            Instruction::JumpFalse(cond, dest) => {
                if self.value(cond) == 0 {
                    self.ip = usize::from(self.value(dest));
                }
            }
            Instruction::Add(reg, a, b) => {
                self.registers[reg] = (self.value(a) + self.value(b)) % 32768
            }
            Instruction::Mul(reg, a, b) => {
                let res = u32::from(self.value(a)) * u32::from(self.value(b));
                self.registers[reg] = (res % 32768) as u16;
            }
            Instruction::Mod(reg, a, b) => {
                self.registers[reg] = self.value(a) % self.value(b);
            }
            Instruction::And(reg, a, b) => {
                self.registers[reg] = self.value(a) & self.value(b);
            }
            Instruction::Or(reg, a, b) => {
                self.registers[reg] = self.value(a) | self.value(b);
            }
            Instruction::Not(reg, a) => {
                self.registers[reg] = (!self.value(a)) & 0x7fff;
            }
            Instruction::Call(a) => {
                self.stack.push(self.ip as u16);
                self.ip = usize::from(self.value(a));
            }
            Instruction::Out(op) => {
                print!("{}", char::from(self.value(op) as u8));
            }
            Instruction::Noop => (),
        };

        Ok(())
    }
}

/// Fetches an instruction with O signature.
macro_rules! fetch_o {
    ($vm:expr, $instr:ident) => {
        Instruction::$instr($vm.fetch_operand())
    };
}

/// Fetches an instruction with R signature.
macro_rules! fetch_r {
    ($vm:expr, $instr:ident) => {
        Instruction::$instr($vm.fetch_register())
    };
}

/// Fetches an instruction with OO signature.
macro_rules! fetch_o2 {
    ($vm:expr, $instr:ident) => {
        Instruction::$instr($vm.fetch_operand(), $vm.fetch_operand())
    };
}

/// Fetches an instruction with RO signature.
macro_rules! fetch_ro {
    ($vm:expr, $instr:ident) => {
        Instruction::$instr($vm.fetch_register(), $vm.fetch_operand())
    };
}

/// Fetches an instruction with ROO signature.
macro_rules! fetch_ro2 {
    ($vm:expr, $instr:ident) => {
        Instruction::$instr(
            $vm.fetch_register(),
            $vm.fetch_operand(),
            $vm.fetch_operand(),
        )
    };
}

impl VirtualMachine {
    /// Fetches the next instruction (opcode + all operands) from memory and advances
    /// the instruction pointer.
    fn fetch(&mut self) -> Instruction {
        let opcode = self.memory[self.ip];
        self.ip += 1;

        match opcode {
            0 => Instruction::Halt,
            1 => fetch_ro!(self, Set),
            2 => fetch_o!(self, Push),
            3 => fetch_r!(self, Pop),
            4 => fetch_ro2!(self, Eq),
            5 => fetch_ro2!(self, GreaterThan),
            6 => fetch_o!(self, Jump),
            7 => fetch_o2!(self, JumpTrue),
            8 => fetch_o2!(self, JumpFalse),
            9 => fetch_ro2!(self, Add),
            10 => fetch_ro2!(self, Mul),
            11 => fetch_ro2!(self, Mod),
            12 => fetch_ro2!(self, And),
            13 => fetch_ro2!(self, Or),
            14 => fetch_ro!(self, Not),
            17 => fetch_o!(self, Call),
            19 => fetch_o!(self, Out),
            21 => Instruction::Noop,
            _ => todo!("unimplemented opcode: {}", opcode),
        }
    }

    /// Fetches the next word from memory, advancing the instruction pointer,
    /// and interprets it as a register index.
    fn fetch_register(&mut self) -> usize {
        match self.fetch_operand() {
            Operand::Reg(reg) => reg,
            _ => panic!("literal used as destination of set opcode"),
        }
    }

    /// Fetches the next operand (word) from memory and advances the instruction pointer.
    fn fetch_operand(&mut self) -> Operand {
        let operand = self.memory[self.ip];
        self.ip += 1;

        if operand < 32768 {
            Operand::Lit(operand)
        } else {
            Operand::Reg(usize::from(operand - 32768))
        }
    }

    /// Unwraps the value contained in this operand, either by returning a literal
    /// or accessing the contents of the specified register.
    fn value(&self, op: Operand) -> u16 {
        match op {
            Operand::Lit(val) => val,
            Operand::Reg(reg) => self.registers[reg],
        }
    }
}

/// A valid decoded instruction, including its operands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    /// Stops execution and terminates the program.
    Halt,
    /// Pushes a word onto the stack.
    Push(Operand),
    /// Removes the top element from the stack and writes it into the destination register.
    Pop(usize),
    /// Sets a register to the operand's value.
    Set(usize, Operand),
    /// Sets a register to 1 if the operands are equal, 0 otherwise.
    Eq(usize, Operand, Operand),
    /// Sets a register to 1 if the 1st operand's value is greater than the 2nd one's, 0 otherwise.
    GreaterThan(usize, Operand, Operand),
    /// Jumps to the specified address in memory.
    Jump(Operand),
    /// Jumps to the destination if the condition is non-zero.
    JumpTrue(Operand, Operand),
    /// Jumps to the destination if the condition is zero.
    JumpFalse(Operand, Operand),
    /// Adds two operands and stores the result in the destination register.
    Add(usize, Operand, Operand),
    /// Multiplies two operands and stores the result in the destination register.
    Mul(usize, Operand, Operand),
    /// Stores the remainder of the two operands in the destination register.
    Mod(usize, Operand, Operand),
    /// Stores the result of the bitwise AND between the two operands in the destination register.
    And(usize, Operand, Operand),
    /// Stores the result of the bitwise OR between the two operands in the destination register.
    Or(usize, Operand, Operand),
    /// Stores the 15-bit bitwise inverse of the operand in the destination register.
    Not(usize, Operand),
    /// Writes the address of the next instruction to the stack and jumpd to the operand.
    Call(Operand),
    /// Writes the character represented by the ASCII code in the operand to stdout.
    Out(Operand),
    /// Does nothing.
    Noop,
}

/// Legal operand types for an instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    /// 15-bit unsigned literal
    Lit(u16),
    /// Register index (0 to 7)
    Reg(usize),
}
