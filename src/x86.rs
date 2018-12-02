//! Intel 64-bit (IA-32e) Disassembler
//!
//! NB: This is _experimental_.  It's hardly complete, and does not currently consider all of the
//! operating modes of the processor.
//!

mod instr;
mod modrm;
mod prefix;
mod register;

pub use self::instr::Instruction;

#[derive(Copy, Clone, Debug, PartialEq)]
crate enum Width {
    Byte,
    Word,
    DWord,
    QWord,
}
