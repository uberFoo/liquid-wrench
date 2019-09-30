//! The Yet-To-Be-Named rust-based, multi-architecture Disassembly Suite
//!
//!
//! *FIXME: Loads of `#[allow(unused_imports)]` and `#![allow(dead_code)]` attributes, scattered
//! throughout the codebase, should eventually be removed.*
#![deny(missing_docs)]
#![allow(dead_code)]
#![feature(non_exhaustive, trace_macros)]

use std::{
    fmt::{self, Write},
    ops::Range,
};

use colored::*;

pub mod x86;

/// Supported Targets
pub enum Targets {
    /// Intel
    X86,
}

/// A struct of bytes, and their disassembly
#[derive(Debug)]
pub struct Disassembly<'a, I> {
    offset: usize,
    bytes: &'a [u8],
    instructions: Vec<ByteSpan<I>>,
}

impl<'a, I> Disassembly<'a, I>
where
    I: fmt::Display,
{
    /// Constructor
    ///
    /// Prepare to disassemble some bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// # use liquid_wrench::Disassembly;
    /// # use liquid_wrench::x86:: Instruction as x86Instruction;
    /// let bytes = vec![0xc3, 0x41, 0x55];
    /// let d: Disassembly<x86Instruction> = Disassembly::new(0x0, &bytes);
    /// ```
    pub fn new(offset: usize, bytes: &'a [u8]) -> Self {
        Disassembly {
            offset,
            bytes,
            instructions: vec![],
        }
    }

    /// Return the bytes associated with an instruction
    ///
    /// # Examples
    ///
    /// ```
    /// # use liquid_wrench::{Disassembly, DisassembleBytes};
    /// # use liquid_wrench::x86::{Disassembler as x86Disassembler, Instruction as x86Instruction};
    /// let bytes = vec![0xc3, 0x41, 0x55];
    /// let mut d = Disassembly::new(0x0, &bytes);
    /// let mut x86 = x86Disassembler::new();
    /// x86.disassemble(&mut d);
    /// let instrs =  d.instructions();
    /// let ret = d.bytes_for_instr(&instrs[0]);
    /// assert_eq!(ret, [0xc3]);
    /// let push = d.bytes_for_instr(&instrs[1]);
    /// assert_eq!(push, [0x41, 0x55]);
    /// ```
    pub fn bytes_for_instr(&self, instr: &ByteSpan<I>) -> &[u8] {
        &self.bytes[instr.bytes.start..instr.bytes.end]
    }

    /// Get a reference to the disassembled instructions
    ///
    pub fn instructions(&self) -> &Vec<ByteSpan<I>> {
        &self.instructions
    }
}

impl<'a, I> fmt::Display for Disassembly<'a, I>
where
    I: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in self.instructions() {
            if let Some(instr) = &i.interpretation {
                let mut s = String::new();
                write!(&mut s, "{:08x}:\t", self.offset + i.bytes.start)?;
                for &b in self.bytes_for_instr(i) {
                    write!(&mut s, "{:02x} ", b)?;
                }

                writeln!(f, "{:<33}{}", s.dimmed().italic(), instr)?;
            } else {
                let mut s = String::new();
                write!(&mut s, "{:08x}:\t", self.offset + i.bytes.start)?;
                for &b in self.bytes_for_instr(i) {
                    write!(&mut s, "{:02x} ", b)?;
                }

                writeln!(f, "{:<33}{:>7}", s.red().italic(), "junk".red().italic())?;
            }
        }
        Ok(())
    }
}

/// A Span  of Bytes
///
/// A means of associating a span of bytes, with some meaning, or interpretation.
#[derive(Debug, PartialEq)]
pub struct ByteSpan<I> {
    interpretation: Option<I>,
    bytes: Range<usize>,
}

/// Disassemble Trait
///
/// This is the main trait that all disassemblers must implement.
pub trait DisassembleBytes<I>
where
    I: fmt::Display,
{
    /// Disassemble bytes, returning a vector of `ByteSpan`s
    fn disassemble(&mut self, disassembly: &mut Disassembly<I>);
}

/// Architecture-agnostic Disassembler Interface
///
/// Currently very notional...
pub struct Disassembler<I> {
    /// The target architecture
    target: Targets,
    /// Where the magic happens
    disassembler: Box<dyn DisassembleBytes<I>>,
    /// The bytes to be disassembled
    bytes: Vec<u8>,
    /// Address to use for the first disassembled instruction
    start_addr: usize,
}

impl<I> Disassembler<I>
where
    I: fmt::Display,
{
    /// The Disassembler constructor.
    pub fn new(
        target: Targets,
        disassembler: Box<dyn DisassembleBytes<I>>,
        bytes: Vec<u8>,
        start_addr: usize,
    ) -> Self {
        Disassembler {
            target,
            disassembler,
            bytes,
            start_addr,
        }
    }

    /// Disassemble Bytes
    ///
    pub fn disassemble(&mut self) -> Disassembly<I> {
        let mut d = Disassembly::new(self.start_addr, &self.bytes);
        self.disassembler.disassemble(&mut d);
        d
    }
}

#[cfg(test)]
mod tests {
    use crate::x86::{Disassembler as x86Disassembler, Instruction as x86Instruction};

    use super::*;

    #[test]
    fn disassembler() {
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

        let x86 = Box::new(x86Disassembler::new());
        let mut disassembler = Disassembler::new(Targets::X86, x86, bin, 0x0);
        let d = disassembler.disassemble();
        println!("{}", d);
    }

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

        println!("{:#?}", x86Instruction::try_parse(&bin[9..]));
    }
}
