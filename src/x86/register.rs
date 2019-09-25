//! Register Definitions
//!
use std::fmt;

use colored::*;

use crate::x86::{
    modrm::REX,
    Width::{self, *},
};

#[derive(Debug, PartialEq)]
pub enum X86Register {
    /// General Purpose Registers
    A,
    B,
    C,
    D,
    SI,
    DI,
    SP,
    BP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    /// Segment Registers
    CS,
    DS,
    SS,
    ES,
    FS,
    GS,
    /// Program Status and Control Register
    EFLAGS,
    /// Instruction Pointer
    EIP,
}

#[derive(Debug, PartialEq)]
crate enum ByteSelector {
    High,
    Low,
}

#[derive(Debug, PartialEq)]
crate struct Register {
    width: Width,
    byte: Option<ByteSelector>,
    reg: X86Register,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // let s = match self.reg {
        let s = match self.width {
            Width::Byte => match self.byte {
                Some(ByteSelector::High) => match self.reg {
                    X86Register::A => "ah",
                    X86Register::B => "bh",
                    X86Register::C => "ch",
                    X86Register::D => "dh",
                    _ => "inconceivable!",
                },
                Some(ByteSelector::Low) => match self.reg {
                    X86Register::A => "al",
                    X86Register::B => "bl",
                    X86Register::C => "cl",
                    X86Register::D => "dl",
                    X86Register::SI => "sil",
                    X86Register::DI => "dil",
                    X86Register::SP => "spl",
                    X86Register::BP => "bpl",
                    X86Register::R8 => "r8b",
                    X86Register::R9 => "r9b",
                    X86Register::R10 => "r10b",
                    X86Register::R11 => "r11b",
                    X86Register::R12 => "r12b",
                    X86Register::R13 => "r13b",
                    X86Register::R14 => "r14b",
                    X86Register::R15 => "r15b",
                    _ => "inconceivable!",
                },
                None => "inconceivable!",
            },
            Width::Word => match self.reg {
                X86Register::A => "ax",
                X86Register::B => "bx",
                X86Register::C => "cx",
                X86Register::D => "dx",
                X86Register::SI => "si",
                X86Register::DI => "di",
                X86Register::SP => "sp",
                X86Register::BP => "bp",
                X86Register::R8 => "r8w",
                X86Register::R9 => "r9w",
                X86Register::R10 => "r10w",
                X86Register::R11 => "r11w",
                X86Register::R12 => "r12w",
                X86Register::R13 => "r13w",
                X86Register::R14 => "r14w",
                X86Register::R15 => "r15w",
                _ => "inconceivable!",
            },
            Width::DWord => match self.reg {
                X86Register::A => "eax",
                X86Register::B => "ebx",
                X86Register::C => "ecx",
                X86Register::D => "edx",
                X86Register::SI => "esi",
                X86Register::DI => "edi",
                X86Register::SP => "esp",
                X86Register::BP => "ebp",
                X86Register::R8 => "r8d",
                X86Register::R9 => "r9d",
                X86Register::R10 => "r10d",
                X86Register::R11 => "r11d",
                X86Register::R12 => "r12d",
                X86Register::R13 => "r13d",
                X86Register::R14 => "r14d",
                X86Register::R15 => "r15d",
                X86Register::EIP => "rip",
                _ => "inconceivable!",
            },
            Width::QWord => match self.reg {
                X86Register::A => "rax",
                X86Register::B => "rbx",
                X86Register::C => "rcx",
                X86Register::D => "rdx",
                X86Register::SI => "rsi",
                X86Register::DI => "rdi",
                X86Register::SP => "rsp",
                X86Register::BP => "rbp",
                X86Register::R8 => "r8",
                X86Register::R9 => "r9",
                X86Register::R10 => "r10",
                X86Register::R11 => "r11",
                X86Register::R12 => "r12",
                X86Register::R13 => "r13",
                X86Register::R14 => "r14",
                X86Register::R15 => "r15",
                X86Register::EIP => "rip",
                _ => "inconceivable!",
            },
        };

        write!(f, "{}", s.yellow())
    }
}

use self::ctors::*;

impl Register {
    /// Decode the REG field of the ModR/M Byte
    ///
    /// Returns an 8-bit register.
    ///
    /// See figure 2-4.
    crate fn r8(b: u8, rex: Option<REX>) -> Self {
        let has_rex = rex.is_some();
        let rex_r = rex.map_or(false, |rex| rex.r);

        reg_8(b, has_rex, rex_r)
    }

    /// Decode the REG field of the ModR/M Byte
    ///
    /// Returns a 16-bit register, unless REX.W is set.  In that case, f64 is invoked, and a 64-bit
    /// register is returned.
    ///
    /// See figure 2-4.
    crate fn r16(b: u8, rex: Option<REX>) -> Self {
        if let Some(r) = rex {
            if r.w {
                return Register::r64(b, rex);
            }
        }
        let rex_r = rex.map_or(false, |rex| rex.r);

        reg_16(b, rex_r)
    }

    /// Decode the REG field of the ModR/M Byte
    ///
    /// Returns a 32-bit register, unless REX.W is set.  In that case, f64 is invoked, and a 64-bit
    /// register is returned.
    ///
    /// See figure 2-4.
    crate fn r32(b: u8, rex: Option<REX>) -> Self {
        if let Some(r) = rex {
            if r.w {
                return Register::r64(b, rex);
            }
        }
        let rex_r = rex.map_or(false, |rex| rex.r);

        reg_32(b, rex_r)
    }

    /// Decode the REG field of the ModR/M Byte
    ///
    /// Returns a 64-bit register.
    ///
    /// See figure 2-4.
    crate fn r64(b: u8, rex: Option<REX>) -> Self {
        let rex_r = rex.map_or(false, |rex| rex.r);

        reg_64(b, rex_r)
    }

    /// Decode an Opcode with the `+rb` encoding.
    ///
    /// Returns an 8-bit register.
    ///
    /// See table 3-1.
    crate fn rb(b: u8, rex: Option<REX>) -> Self {
        let has_rex = rex.is_some();
        let rex_b = rex.map_or(false, |rex| rex.b);

        reg_8(b, has_rex, rex_b)
    }

    /// Decode an Opcode with the `+rw` encoding.
    ///
    /// Returns a 16-bit register, unless REX.W is set.  In that case, f64 is invoked, and a 64-bit
    /// register is returned.
    ///
    /// See table 3-1.
    crate fn rw(b: u8, rex: Option<REX>) -> Self {
        if let Some(r) = rex {
            if r.w {
                return Register::ro(b, rex);
            }
        }
        let rex_b = rex.map_or(false, |rex| rex.b);

        reg_16(b, rex_b)
    }

    /// Decode an Opcode with the `+rd` encoding.
    ///
    /// Returns a 32-bit register, unless REX.W is set.  In that case, f64 is invoked, and a 64-bit
    /// register is returned.
    ///
    /// See table 3-1.
    crate fn rd(b: u8, rex: Option<REX>) -> Self {
        if let Some(r) = rex {
            if r.w {
                return Register::ro(b, rex);
            }
        }
        let rex_b = rex.map_or(false, |rex| rex.b);

        reg_32(b, rex_b)
    }

    /// Decode an Opcode with the `+ro` encoding.
    ///
    /// Returns a 64-bit register.
    ///
    /// See table 3-1.
    crate fn ro(b: u8, rex: Option<REX>) -> Self {
        let rex_b = rex.map_or(false, |rex| rex.b);

        reg_64(b, rex_b)
    }
}

/// Return an 8-bit Register
///
fn reg_8(b: u8, has_rex: bool, rex_bit: bool) -> Register {
    match (b, has_rex, rex_bit) {
        // The following always translate this way, regardless of REX.
        (0, _, false) => al(),
        (1, _, false) => cl(),
        (2, _, false) => dl(),
        (3, _, false) => bl(),

        // These apply if there is no REX byte present.
        (4, false, false) => ah(),
        (5, false, false) => ch(),
        (6, false, false) => dh(),
        (7, false, false) => bh(),

        // These are identical to above, but are accessible only in the presence of REX.
        (4, true, false) => spl(),
        (5, true, false) => bpl(),
        (6, true, false) => sil(),
        (7, true, false) => dil(),

        (0, true, true) => r8b(),
        (1, true, true) => r9b(),
        (2, true, true) => r10b(),
        (3, true, true) => r11b(),
        (4, true, true) => r12b(),
        (5, true, true) => r13b(),
        (6, true, true) => r14b(),
        (7, true, true) => r15b(),

        // FIXME: Probably should not panic, and throw some kind of parsing error.
        (_, _, _) => panic!(
            "reg_8: bad register encoding ({}, {}, {})",
            b, has_rex, rex_bit
        ),
    }
}

fn reg_16(b: u8, rex_bit: bool) -> Register {
    match (b, rex_bit) {
        (0, false) => ax(),
        (1, false) => cx(),
        (2, false) => dx(),
        (3, false) => bx(),
        (4, false) => sp(),
        (5, false) => bp(),
        (6, false) => si(),
        (7, false) => di(),
        (0, true) => r8w(),
        (1, true) => r9w(),
        (2, true) => r10w(),
        (3, true) => r11w(),
        (4, true) => r12w(),
        (5, true) => r13w(),
        (6, true) => r14w(),
        (7, true) => r15w(),

        // FIXME: Probably should not panic, and throw some kind of parsing error.
        (_, _) => panic!("reg_16: bad register encoding ({}, {})", b, rex_bit),
    }
}

fn reg_32(b: u8, rex_bit: bool) -> Register {
    match (b, rex_bit) {
        (0, false) => eax(),
        (1, false) => ecx(),
        (2, false) => edx(),
        (3, false) => ebx(),
        (4, false) => esp(),
        (5, false) => ebp(),
        (6, false) => esi(),
        (7, false) => edi(),
        (0, true) => r8d(),
        (1, true) => r9d(),
        (2, true) => r10d(),
        (3, true) => r11d(),
        (4, true) => r12d(),
        (5, true) => r13d(),
        (6, true) => r14d(),
        (7, true) => r15d(),

        // FIXME: Probably should not panic, and throw some kind of parsing error.
        (_, _) => panic!("reg_32: bad register encoding ({}, {})", b, rex_bit),
    }
}

fn reg_64(b: u8, rex_bit: bool) -> Register {
    match (b, rex_bit) {
        (0, false) => rax(),
        (1, false) => rcx(),
        (2, false) => rdx(),
        (3, false) => rbx(),
        (4, false) => rsp(),
        (5, false) => rbp(),
        (6, false) => rsi(),
        (7, false) => rdi(),
        (0, true) => r8(),
        (1, true) => r9(),
        (2, true) => r10(),
        (3, true) => r11(),
        (4, true) => r12(),
        (5, true) => r13(),
        (6, true) => r14(),
        (7, true) => r15(),

        // FIXME: Probably should not panic, and throw some kind of parsing error.
        (_, _) => panic!("reg_64: bad register encoding ({}, {})", b, rex_bit),
    }
}

crate mod ctors {
    use super::{ByteSelector::*, X86Register::*, *};

    crate fn rip() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: EIP,
        }
    }

    crate fn al() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: A,
        }
    }

    crate fn bl() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: B,
        }
    }

    crate fn cl() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: C,
        }
    }

    crate fn dl() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: D,
        }
    }

    crate fn ah() -> Register {
        Register {
            width: Byte,
            byte: Some(High),
            reg: A,
        }
    }

    crate fn bh() -> Register {
        Register {
            width: Byte,
            byte: Some(High),
            reg: B,
        }
    }

    crate fn ch() -> Register {
        Register {
            width: Byte,
            byte: Some(High),
            reg: C,
        }
    }

    crate fn dh() -> Register {
        Register {
            width: Byte,
            byte: Some(High),
            reg: D,
        }
    }

    crate fn spl() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: SI,
        }
    }

    crate fn bpl() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: SI,
        }
    }

    crate fn sil() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: SI,
        }
    }
    crate fn dil() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: SI,
        }
    }

    crate fn r8b() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R8,
        }
    }

    crate fn r9b() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R9,
        }
    }

    crate fn r10b() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R10,
        }
    }

    crate fn r11b() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R11,
        }
    }

    crate fn r12b() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R12,
        }
    }

    crate fn r13b() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R13,
        }
    }

    crate fn r14b() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R14,
        }
    }

    crate fn r15b() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R15,
        }
    }

    crate fn ax() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: A,
        }
    }

    crate fn bx() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: B,
        }
    }

    crate fn cx() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: C,
        }
    }

    crate fn dx() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: D,
        }
    }

    crate fn si() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: SI,
        }
    }

    crate fn di() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: DI,
        }
    }

    crate fn sp() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: SP,
        }
    }

    crate fn bp() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: BP,
        }
    }

    crate fn r8w() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: R8,
        }
    }

    crate fn r9w() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: R9,
        }
    }

    crate fn r10w() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: R10,
        }
    }

    crate fn r11w() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: R11,
        }
    }

    crate fn r12w() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: R12,
        }
    }

    crate fn r13w() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: R13,
        }
    }

    crate fn r14w() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: R14,
        }
    }

    crate fn r15w() -> Register {
        Register {
            width: Word,
            byte: None,
            reg: R15,
        }
    }

    crate fn eax() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: A,
        }
    }

    crate fn ebx() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: B,
        }
    }

    crate fn ecx() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: C,
        }
    }

    crate fn edx() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: D,
        }
    }

    crate fn esi() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: SI,
        }
    }

    crate fn edi() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: DI,
        }
    }

    crate fn esp() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: SP,
        }
    }

    crate fn ebp() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: BP,
        }
    }

    crate fn r8d() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: R8,
        }
    }

    crate fn r9d() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: R9,
        }
    }

    crate fn r10d() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: R10,
        }
    }

    crate fn r11d() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: R11,
        }
    }

    crate fn r12d() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: R12,
        }
    }

    crate fn r13d() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: R13,
        }
    }

    crate fn r14d() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: R14,
        }
    }

    crate fn r15d() -> Register {
        Register {
            width: DWord,
            byte: None,
            reg: R15,
        }
    }

    crate fn rax() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: A,
        }
    }

    crate fn rbx() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: B,
        }
    }

    crate fn rcx() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: C,
        }
    }

    crate fn rdx() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: D,
        }
    }

    crate fn rsi() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: SI,
        }
    }

    crate fn rdi() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: DI,
        }
    }

    crate fn rsp() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: SP,
        }
    }

    crate fn rbp() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: BP,
        }
    }

    crate fn r8() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: R8,
        }
    }

    crate fn r9() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: R9,
        }
    }

    crate fn r10() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: R10,
        }
    }

    crate fn r11() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: R11,
        }
    }

    crate fn r12() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: R12,
        }
    }

    crate fn r13() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: R13,
        }
    }

    crate fn r14() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: R14,
        }
    }

    crate fn r15() -> Register {
        Register {
            width: QWord,
            byte: None,
            reg: R15,
        }
    }
}
