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
            Instruction::Push(op) => self.stack.push(self.value(op)),
            Instruction::Pop(reg) => self.registers[reg] = self.stack.pop().unwrap(),
            Instruction::Set(reg, value) => self.registers[reg] = self.value(value),
            Instruction::Eq(reg, a, b) => {
                self.registers[reg] = if self.value(a) == self.value(b) { 1 } else { 0 };
            }
            Instruction::Jump(dest) => self.ip = usize::from(self.value(dest)),
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
            Instruction::Add(reg, lhs, rhs) => {
                self.registers[reg] = (self.value(lhs) + self.value(rhs)) % 32768
            }
            Instruction::Out(op) => {
                print!("{}", char::from(self.value(op) as u8));
            }
            Instruction::Noop => (),
        };

        Ok(())
    }

    /// Fetches the next instruction (opcode + all operands) from memory and advances
    /// the instruction pointer.
    fn fetch(&mut self) -> Instruction {
        let opcode = self.memory[self.ip];
        self.ip += 1;

        match opcode {
            0 => Instruction::Halt,
            1 => Instruction::Set(self.fetch_register(), self.fetch_operand()),
            2 => Instruction::Push(self.fetch_operand()),
            3 => Instruction::Pop(self.fetch_register()),
            4 => Instruction::Eq(
                self.fetch_register(),
                self.fetch_operand(),
                self.fetch_operand(),
            ),
            6 => Instruction::Jump(self.fetch_operand()),
            7 => Instruction::JumpTrue(self.fetch_operand(), self.fetch_operand()),
            8 => Instruction::JumpFalse(self.fetch_operand(), self.fetch_operand()),
            9 => Instruction::Add(
                self.fetch_register(),
                self.fetch_operand(),
                self.fetch_operand(),
            ),
            19 => Instruction::Out(self.fetch_operand()),
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
    /// Jumps to the specified address in memory.
    Jump(Operand),
    /// Jumps to the destination if the condition is non-zero.
    JumpTrue(Operand, Operand),
    /// Jumps to the destination if the condition is zero.
    JumpFalse(Operand, Operand),
    /// Adds two operands and stores the result in the destination register.
    Add(usize, Operand, Operand),
    /// Write the character represented by the ASCII code in the operand to stdout.
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
