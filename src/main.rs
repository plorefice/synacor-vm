use std::{fs::File, io::Read, path::PathBuf};

use anyhow::Result;
use structopt::StructOpt;
use synacor::VirtualMachine;

#[derive(StructOpt)]
struct Cli {
    /// Loads a save state and resumes execution
    #[structopt(short, long, parse(from_os_str))]
    load_state: Option<PathBuf>,

    /// Binary file containing the program to run
    #[structopt(parse(from_os_str))]
    program: PathBuf,
}

fn main() -> Result<()> {
    let args = Cli::from_args();

    let mut vm = VirtualMachine::new();

    let mut program = Vec::new();
    File::open(args.program)?.read_to_end(&mut program)?;

    vm.run(&program);

    Ok(())
}
