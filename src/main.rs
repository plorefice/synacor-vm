#[derive(Debug, Clone)]
pub struct VirtualMachine {
    registers: [u16; 8],
    memory: [u16; 32768],
    _stack: Vec<u16>,

    ip: usize, // instruction pointer
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self {
            registers: Default::default(),
            memory: [0; 32768],
            _stack: Default::default(),

            ip: 0,
        }
    }
}

impl VirtualMachine {
    // Creates a new, fresh virtual machine instance, with no loaded program.
    pub fn new() -> Self {
        Default::default()
    }

    // Loads and run a program, returning in case an halt instruction is encountered or in case
    // an error occurs.
    pub fn run(&mut self, program: &[u8]) {
        self.load(program);

        loop {
            if self.step().is_err() {
                return;
            }
        }
    }

    // Loads the program file into memory, starting address 0.
    fn load(&mut self, program: &[u8]) {
        let mut words = program.chunks_exact(2).enumerate();

        while let Some((i, &[lo, hi])) = words.next() {
            self.memory[i] = u16::from(hi) << 8 | u16::from(lo);
        }
    }

    // Performs a single step of the VM, fetching, decoding and executing the next instruction.
    fn step(&mut self) -> Result<(), ()> {
        match self.fetch() {
            Instruction::Halt => return Err(()),
            Instruction::Noop => (),
            Instruction::Out(op) => {
                print!("{}", char::from(op.value(self) as u8));
            }
        };

        Ok(())
    }

    // Fetches the next instruction (opcode + all operands) from memory and advances
    // the instruction pointer.
    fn fetch(&mut self) -> Instruction {
        let opcode = self.memory[self.ip];
        self.ip += 1;

        match opcode {
            0 => Instruction::Halt,
            19 => Instruction::Out(self.fetch_operand()),
            21 => Instruction::Noop,
            _ => todo!("unimplemented opcode: {}", opcode),
        }
    }

    // Fetches the next operand (word) from memory and advances the instruction pointer.
    fn fetch_operand(&mut self) -> Operand {
        let operand = self.memory[self.ip];
        self.ip += 1;

        if operand < 32768 {
            Operand::Lit(operand)
        } else {
            Operand::Reg(usize::from(operand - 32768))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Halt,
    Noop,
    Out(Operand),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    Lit(u16),
    Reg(usize),
}

impl Operand {
    pub fn value(&self, vm: &VirtualMachine) -> u16 {
        match *self {
            Operand::Lit(val) => val,
            Operand::Reg(reg) => vm.registers[reg],
        }
    }
}

fn main() {
    let mut vm = VirtualMachine::new();

    vm.run(include_bytes!("../inputs/challenge.bin"));
}
