use synacor::VirtualMachine;

fn main() {
    let mut vm = VirtualMachine::new();

    vm.run(include_bytes!("../inputs/challenge.bin"));
}
