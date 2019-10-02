use {
    failure,
    liquid_wrench::{binary::Binary, x86::Disassembler as X86Disassembler, Disassembler, Targets},
    std::{fs::File, io::Read, path::PathBuf},
    structopt::{clap::AppSettings, StructOpt},
};

pub fn main() -> Result<(), failure::Error> {
    let prog_opts = ProgramOptions::from_args();

    let mut bytes = vec![];
    File::open(&prog_opts.bin_path)?.read_to_end(&mut bytes)?;

    let binary = Binary::new(bytes)?;
    for (name, section) in binary.sections {
        let x86 = Box::new(X86Disassembler::new());
        let mut disassembler = Disassembler::new(Targets::X86, x86, section.bytes, section.offset);
        let d = disassembler.disassemble();
        println!("Disassembly for section {:?}", name);
        println!("{}\n", d);
    }

    Ok(())
}

#[derive(StructOpt, Debug)]
#[structopt(
    name = "wolverine",
    about = "Disassemble X86 binary",
    global_settings(&[AppSettings::ColoredHelp]))]
pub struct ProgramOptions {
    /// Path to binary
    pub bin_path: PathBuf,
}
