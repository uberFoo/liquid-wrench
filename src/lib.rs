//! The Yet-To-Be-Named rust-based, multi-architecture Disassembly Suite
//!
//!
//! *FIXME: Loads of `#[allow(unused_imports)]` and `#![allow(dead_code)]` attributes, scattered
//! throughout the codebase, should eventually be removed.*
#![deny(missing_docs)]
#![allow(dead_code)]
#![feature(crate_visibility_modifier, non_exhaustive, trace_macros)]

#[allow(unused_imports)]
use std::fmt::Display;

pub mod x86;

#[allow(unused_imports)]
use crate::x86::Instruction as x86Instruction;

/// Supported Targets
pub enum Targets {
    /// Intel
    X86,
}

/// A Span  of Bytes
///
/// A means of associating a span of bytes, with some meaning, or interpretation.
#[derive(Debug, PartialEq)]
pub struct ByteSpan<'a, I> {
    interpretation: Option<I>,
    bytes: &'a [u8],
}

/// Architecture-agnostic Disassembler Interface
///
/// Currently very notional...
pub struct Disassembler {
    /// The target architecture
    target: Targets,
    /// The bytes to be disassembled
    bytes: Vec<u8>,
    /// Address to use for the first disassembled instruction
    start_addr: u64,
}

impl Disassembler {
    /// The Disassembler constructor.
    pub fn new(target: Targets, bytes: Vec<u8>, start_addr: u64) -> Self {
        Disassembler {
            target,
            bytes,
            start_addr,
        }
    }
}

// pub trait Instruction: Display {
//     fn from_bytes(input: &mut Vec<u8>) -> Self;
// }

// pub struct DisassemblerIterator {

// }

// impl Iterator for Disassembler {
//     type Item = Instruction;
//     fn next(&mut self) -> Option<Instruction> {
//         None
//     }
// }

// impl IntoIterator for Disassembler {
//     type Item = Instruction;
//     type
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn disassembly() {
        let bin = vec![
            0x31, 0xed, // xor    %ebp,%ebp
            0x49, 0x89, 0xd1, // mov    %rdx,%r9
            0x5e, // pop    %rsi
            0x48, 0x89, 0xe2, // mov    %rsp,%rdx
            0x48, 0x83, 0xe4, 0xf0, // and    $0xfffffffffffffff0,%rsp
            0x50, // push   %rax
            0x54, // push   %rsp
            0x4c, 0x8d, 0x05, 0xca, 0x01, 0x00, 0x00, // lea    0x1ca(%rip),%r8
            0x48, 0x8d, 0x0d, 0x53, 0x01, 0x00, 0x00, // lea    0x153(%rip),%cx
            0x48, 0x8d, 0x3d, 0x33, 0x01, 0x00, 0x00, // lea    0x133(%rip),%rdi
            0xff, 0x15, 0xee, 0x09, 0x20, 0x00, // callq  *0x2009ee(%rip)
            0xf4, // hlt
            0x0f, 0x1f, 0x44, 0x00, 0x00, // nopl 0x0(%rax,%rax,1)
        ];

        println!("{:?}", x86Instruction::try_parse(&bin[9..]));
    }
}
