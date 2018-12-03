#![allow(dead_code)]
//! Register Definitions
//!
use crate::x86::{
    modrm::REX,
    Width::{self, *},
};

#[derive(Debug, PartialEq)]
pub enum X86Register {
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
        // The followirg always translate this way, regardless of REX.
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

        (0, true, true) => r8l(),
        (1, true, true) => r9l(),
        (2, true, true) => r10l(),
        (3, true, true) => r11l(),
        (4, true, true) => r12l(),
        (5, true, true) => r13l(),
        (6, true, true) => r14l(),
        (7, true, true) => r15l(),

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

    crate fn r8l() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R8,
        }
    }

    crate fn r9l() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R9,
        }
    }

    crate fn r10l() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R10,
        }
    }

    crate fn r11l() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R11,
        }
    }

    crate fn r12l() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R12,
        }
    }

    crate fn r13l() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R13,
        }
    }

    crate fn r14l() -> Register {
        Register {
            width: Byte,
            byte: Some(Low),
            reg: R14,
        }
    }

    crate fn r15l() -> Register {
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
