use std::{fs::File, io::Read, path::PathBuf};

use {
    failure::Error,
    goblin::Object,
    structopt::{clap::AppSettings, StructOpt},
};

use liquid_wrench::{x86::Disassembler as X86Disassembler, Disassembler, Targets};

pub fn main() -> Result<(), Error> {
    let prog_opts = ProgramOptions::from_args();

    let mut bytes = vec![];
    File::open(&prog_opts.bin_path)?.read_to_end(&mut bytes)?;

    let (offset, instr_bytes) = match Object::parse(bytes.as_slice())? {
        Object::Mach(mach) => parse_mach(mach),
        _ => panic!("I only deal with MacOS binaries for now."),
    };

    let x86 = Box::new(X86Disassembler::new());
    let mut disassembler = Disassembler::new(Targets::X86, x86, instr_bytes, offset);
    let d = disassembler.disassemble();
    d.print();

    Ok(())
}

fn parse_mach(binary: goblin::mach::Mach) -> (usize, Vec<u8>) {
    let mut __TEXT = [
        0x5F, 0x5F, 0x54, 0x45, 0x58, 0x54, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00,
    ];
    let mut __text = [
        0x5F, 0x5F, 0x74, 0x65, 0x78, 0x74, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00,
    ];

    let mut bytes = vec![];
    let mut offset = 0;
    if let goblin::mach::Mach::Binary(mach) = binary {
        for seg in &mach.segments {
            if seg.segname == __TEXT {
                if let Ok(sections) = seg.sections() {
                    for sect in sections {
                        if sect.0.sectname == __text {
                            offset = sect.0.offset;
                            bytes.extend_from_slice(sect.1);
                        }
                    }
                }
            }
        }
    }

    (offset as usize, bytes)
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
