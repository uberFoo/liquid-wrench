//! Intel 64-bit (IA-32e) Disassembler
//!
//! NB: This is _experimental_.  It's hardly complete, and does not currently consider all of the
//! operating modes of the processor.
//!
#[macro_use]
mod macros;

mod instr;
mod modrm;
mod prefix;
mod register;

pub use self::instr::Instruction;

use crate::{x86::instr::InstructionDecoder, DisassembleBytes, Disassembly};

#[derive(Copy, Clone, Debug, PartialEq)]
crate enum Width {
    Byte,
    Word,
    DWord,
    QWord,
}

/// x86 Specific Disassembler
///
pub struct Disassembler {
    // bytes: &'a mut Disassembly<Instruction>,
}

impl Disassembler {
    /// Disassembler Constructor
    ///
    pub fn new() -> Self {
        Disassembler {}
    }
}

impl DisassembleBytes<Instruction> for Disassembler {
    fn disassemble(&mut self, disassembly: &mut Disassembly<Instruction>) {
        for i in InstructionDecoder::new(&disassembly.bytes) {
            disassembly.instructions.push(i)
        }
    }
}
