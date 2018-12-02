//! Register Definitions
//!
use crate::x86::{
    modrm::REX,
    Width::{self, *},
};

/// 8-bit General-Purpose Registers
///
/// AL, BL, CL, DL, AH, BH, CH, and DH are available without a REX prefix.
/// AL, BL, CL, DL, SIL, DIL, SPL, BPL and R8L-R15L require a REX prefix.
#[derive(Debug, PartialEq)]
crate enum Register8 {
    AL,
    BL,
    CL,
    DL,
    AH,
    BH,
    CH,
    DH,
    SIL,
    DIL,
    SPL,
    BPL,
    R8L,
    R9L,
    R10L,
    R11L,
    R12L,
    R13L,
    R14L,
    R15L,
}

/// 16-bit General-Purpose Registers
///
#[derive(Debug, PartialEq)]
crate enum Register16 {
    AX,
    BX,
    CX,
    DX,
    SI,
    DI,
    SP,
    BP,
    R8W,
    R9W,
    R10W,
    R11W,
    R12W,
    R13W,
    R14W,
    R15W,
}

/// 32-bit General-Purpose Registers
///
#[derive(Debug, PartialEq)]
crate enum Register32 {
    EAX,
    EBX,
    ECX,
    EDX,
    ESI,
    EDI,
    ESP,
    EBP,
    R8D,
    R9D,
    R10D,
    R11D,
    R12D,
    R13D,
    R14D,
    R15D,
}

/// 64-bit General-Purpose Registers
///
#[derive(Debug, PartialEq)]
crate enum Register64 {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RSP,
    RBP,
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
crate enum Register {
    Byte(Register8),
    Word(Register16),
    DWord(Register32),
    QWord(Register64),
    IP,
}

impl Register {
    /// Decode the REG field of the ModR/M Byte
    ///
    /// Returns an 8-bit register.
    ///
    /// See figure 2-4.
    crate fn r8(b: u8, rex: Option<REX>) -> Self {
        let has_rex = rex.is_some();
        let rex_r = rex.map_or(false, |rex| rex.r);

        match (b, has_rex, rex_r) {
            (0, false, false) => Register::Byte(Register8::AL),
            (1, false, false) => Register::Byte(Register8::CL),
            (2, false, false) => Register::Byte(Register8::DL),
            (3, false, false) => Register::Byte(Register8::BL),
            (4, false, false) => Register::Byte(Register8::AH),
            (5, false, false) => Register::Byte(Register8::CH),
            (6, false, false) => Register::Byte(Register8::DH),
            (7, false, false) => Register::Byte(Register8::BH),

            (4, true, false) => Register::Byte(Register8::SPL),
            (5, true, false) => Register::Byte(Register8::BPL),
            (6, true, false) => Register::Byte(Register8::SIL),
            (7, true, false) => Register::Byte(Register8::DIL),

            (0, true, true) => Register::Byte(Register8::R8L),
            (1, true, true) => Register::Byte(Register8::R9L),
            (2, true, true) => Register::Byte(Register8::R10L),
            (3, true, true) => Register::Byte(Register8::R11L),
            (4, true, true) => Register::Byte(Register8::R12L),
            (5, true, true) => Register::Byte(Register8::R13L),
            (6, true, true) => Register::Byte(Register8::R14L),
            (7, true, true) => Register::Byte(Register8::R15L),

            // FIXME: Probably should not panic, and throw some kind of parsing error.
            (_, _, _) => panic!("bad register encoding"),
        }
    }

    /// Decode the REG field of the ModR/M Byte
    ///
    /// Returns a 16-bit register.
    ///
    /// See figure 2-4.
    crate fn r16(b: u8, rex: Option<REX>) -> Self {
        let rex_r = rex.map_or(false, |rex| rex.r);

        match (b, rex_r) {
            (0, false) => Register::Word(Register16::AX),
            (1, false) => Register::Word(Register16::CX),
            (2, false) => Register::Word(Register16::DX),
            (3, false) => Register::Word(Register16::BX),
            (4, false) => Register::Word(Register16::SP),
            (5, false) => Register::Word(Register16::BP),
            (6, false) => Register::Word(Register16::SI),
            (7, false) => Register::Word(Register16::DI),
            (0, true) => Register::Word(Register16::R8W),
            (1, true) => Register::Word(Register16::R9W),
            (2, true) => Register::Word(Register16::R10W),
            (3, true) => Register::Word(Register16::R11W),
            (4, true) => Register::Word(Register16::R12W),
            (5, true) => Register::Word(Register16::R13W),
            (6, true) => Register::Word(Register16::R14W),
            (7, true) => Register::Word(Register16::R15W),

            // FIXME: Probably should not panic, and throw some kind of parsing error.
            (_, _) => panic!("bad register encoding"),
        }
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

        match (b, rex_r) {
            (0, false) => Register::DWord(Register32::EAX),
            (1, false) => Register::DWord(Register32::ECX),
            (2, false) => Register::DWord(Register32::EDX),
            (3, false) => Register::DWord(Register32::EBX),
            (4, false) => Register::DWord(Register32::ESP),
            (5, false) => Register::DWord(Register32::EBP),
            (6, false) => Register::DWord(Register32::ESI),
            (7, false) => Register::DWord(Register32::EDI),
            (0, true) => Register::DWord(Register32::R8D),
            (1, true) => Register::DWord(Register32::R9D),
            (2, true) => Register::DWord(Register32::R10D),
            (3, true) => Register::DWord(Register32::R11D),
            (4, true) => Register::DWord(Register32::R12D),
            (5, true) => Register::DWord(Register32::R13D),
            (6, true) => Register::DWord(Register32::R14D),
            (7, true) => Register::DWord(Register32::R15D),

            // FIXME: Probably should not panic, and throw some kind of parsing error.
            (_, _) => panic!("bad register encoding"),
        }
    }

    /// Decode the REG field of the ModR/M Byte
    ///
    /// Returns a 64-bit register.
    ///
    /// See figure 2-4.
    crate fn r64(b: u8, rex: Option<REX>) -> Self {
        let rex_r = rex.map_or(false, |rex| rex.r);

        match (b, rex_r) {
            (0, false) => Register::QWord(Register64::RAX),
            (1, false) => Register::QWord(Register64::RCX),
            (2, false) => Register::QWord(Register64::RDX),
            (3, false) => Register::QWord(Register64::RBX),
            (4, false) => Register::QWord(Register64::RSP),
            (5, false) => Register::QWord(Register64::RBP),
            (6, false) => Register::QWord(Register64::RSI),
            (7, false) => Register::QWord(Register64::RDI),
            (0, true) => Register::QWord(Register64::R8),
            (1, true) => Register::QWord(Register64::R9),
            (2, true) => Register::QWord(Register64::R10),
            (3, true) => Register::QWord(Register64::R11),
            (4, true) => Register::QWord(Register64::R12),
            (5, true) => Register::QWord(Register64::R13),
            (6, true) => Register::QWord(Register64::R14),
            (7, true) => Register::QWord(Register64::R15),

            // FIXME: Probably should not panic, and throw some kind of parsing error.
            (_, _) => panic!("bad register encoding"),
        }
    }

    /// Decode an Opcode with the `+rb` encoding.
    ///
    /// Returns an 8-bit register.
    ///
    /// See table 3-1.
    crate fn rb(b: u8, rex: Option<REX>) -> Self {
        let has_rex = rex.is_some();
        let rex_b = rex.map_or(false, |rex| rex.b);

        match (b, has_rex, rex_b) {
            (0, false, false) => Register::Byte(Register8::AL),
            (1, false, false) => Register::Byte(Register8::CL),
            (2, false, false) => Register::Byte(Register8::DL),
            (3, false, false) => Register::Byte(Register8::BL),
            (4, false, false) => Register::Byte(Register8::AH),
            (5, false, false) => Register::Byte(Register8::CH),
            (6, false, false) => Register::Byte(Register8::DH),
            (7, false, false) => Register::Byte(Register8::BH),

            (4, true, false) => Register::Byte(Register8::SPL),
            (5, true, false) => Register::Byte(Register8::BPL),
            (6, true, false) => Register::Byte(Register8::SIL),
            (7, true, false) => Register::Byte(Register8::DIL),

            (0, true, true) => Register::Byte(Register8::R8L),
            (1, true, true) => Register::Byte(Register8::R9L),
            (2, true, true) => Register::Byte(Register8::R10L),
            (3, true, true) => Register::Byte(Register8::R11L),
            (4, true, true) => Register::Byte(Register8::R12L),
            (5, true, true) => Register::Byte(Register8::R13L),
            (6, true, true) => Register::Byte(Register8::R14L),
            (7, true, true) => Register::Byte(Register8::R15L),

            // FIXME: Probably should not panic, and throw some kind of parsing error.
            (_, _, _) => panic!("bad register encoding"),
        }
    }

    /// Decode an Opcode with the `+rw` encoding.
    ///
    /// Returns a 16-bit register.
    ///
    /// See table 3-1.
    crate fn rw(b: u8, rex: Option<REX>) -> Self {
        let rex_b = rex.map_or(false, |rex| rex.b);

        match (b, rex_b) {
            (0, false) => Register::Word(Register16::AX),
            (1, false) => Register::Word(Register16::CX),
            (2, false) => Register::Word(Register16::DX),
            (3, false) => Register::Word(Register16::BX),
            (4, false) => Register::Word(Register16::SP),
            (5, false) => Register::Word(Register16::BP),
            (6, false) => Register::Word(Register16::SI),
            (7, false) => Register::Word(Register16::DI),
            (0, true) => Register::Word(Register16::R8W),
            (1, true) => Register::Word(Register16::R9W),
            (2, true) => Register::Word(Register16::R10W),
            (3, true) => Register::Word(Register16::R11W),
            (4, true) => Register::Word(Register16::R12W),
            (5, true) => Register::Word(Register16::R13W),
            (6, true) => Register::Word(Register16::R14W),
            (7, true) => Register::Word(Register16::R15W),

            // FIXME: Probably should not panic, and throw some kind of parsing error.
            (_, _) => panic!("bad register encoding"),
        }
    }

    /// Decode an Opcode with the `+rd` encoding.
    ///
    /// Returns a 32-bit register.
    ///
    /// See table 3-1.
    crate fn rd(b: u8, rex: Option<REX>) -> Self {
        let rex_b = rex.map_or(false, |rex| rex.b);

        match (b, rex_b) {
            (0, false) => Register::DWord(Register32::EAX),
            (1, false) => Register::DWord(Register32::ECX),
            (2, false) => Register::DWord(Register32::EDX),
            (3, false) => Register::DWord(Register32::EBX),
            (4, false) => Register::DWord(Register32::ESP),
            (5, false) => Register::DWord(Register32::EBP),
            (6, false) => Register::DWord(Register32::ESI),
            (7, false) => Register::DWord(Register32::EDI),
            (0, true) => Register::DWord(Register32::R8D),
            (1, true) => Register::DWord(Register32::R9D),
            (2, true) => Register::DWord(Register32::R10D),
            (3, true) => Register::DWord(Register32::R11D),
            (4, true) => Register::DWord(Register32::R12D),
            (5, true) => Register::DWord(Register32::R13D),
            (6, true) => Register::DWord(Register32::R14D),
            (7, true) => Register::DWord(Register32::R15D),

            // FIXME: Probably should not panic, and throw some kind of parsing error.
            (_, _) => panic!("bad register encoding"),
        }
    }

    /// Decode an Opcode with the `+ro` encoding.
    ///
    /// Returns a 64-bit register.
    ///
    /// See table 3-1.
    crate fn ro(b: u8, rex: Option<REX>) -> Self {
        let rex_b = rex.map_or(false, |rex| rex.b);

        match (b, rex_b) {
            (0, false) => Register::QWord(Register64::RAX),
            (1, false) => Register::QWord(Register64::RCX),
            (2, false) => Register::QWord(Register64::RDX),
            (3, false) => Register::QWord(Register64::RBX),
            (4, false) => Register::QWord(Register64::RSP),
            (5, false) => Register::QWord(Register64::RBP),
            (6, false) => Register::QWord(Register64::RSI),
            (7, false) => Register::QWord(Register64::RDI),
            (0, true) => Register::QWord(Register64::R8),
            (1, true) => Register::QWord(Register64::R9),
            (2, true) => Register::QWord(Register64::R10),
            (3, true) => Register::QWord(Register64::R11),
            (4, true) => Register::QWord(Register64::R12),
            (5, true) => Register::QWord(Register64::R13),
            (6, true) => Register::QWord(Register64::R14),
            (7, true) => Register::QWord(Register64::R15),

            // FIXME: Probably should not panic, and throw some kind of parsing error.
            (_, _) => panic!("bad register encoding"),
        }
    }

    crate fn decode(b: u8, width: Width) -> Self {
        match (width, b) {
            (Byte, 0) => Register::Byte(Register8::AL),
            (Byte, 1) => Register::Byte(Register8::CL),
            (Byte, 2) => Register::Byte(Register8::DL),
            (Byte, 3) => Register::Byte(Register8::BL),
            (Byte, 4) => Register::Byte(Register8::AH),
            (Byte, 5) => Register::Byte(Register8::CH),
            (Byte, 6) => Register::Byte(Register8::DH),
            (Byte, 7) => Register::Byte(Register8::BH),
            (Byte, 8) => Register::Byte(Register8::R8L),
            (Byte, 9) => Register::Byte(Register8::R9L),
            (Byte, 10) => Register::Byte(Register8::R10L),
            (Byte, 11) => Register::Byte(Register8::R11L),
            (Byte, 12) => Register::Byte(Register8::R12L),
            (Byte, 13) => Register::Byte(Register8::R13L),
            (Byte, 14) => Register::Byte(Register8::R14L),
            (Byte, 15) => Register::Byte(Register8::R15L),

            (Word, 0) => Register::Word(Register16::AX),
            (Word, 1) => Register::Word(Register16::CX),
            (Word, 2) => Register::Word(Register16::DX),
            (Word, 3) => Register::Word(Register16::BX),
            (Word, 4) => Register::Word(Register16::SP),
            (Word, 5) => Register::Word(Register16::BP),
            (Word, 6) => Register::Word(Register16::SI),
            (Word, 7) => Register::Word(Register16::DI),
            (Word, 8) => Register::Word(Register16::R8W),
            (Word, 9) => Register::Word(Register16::R9W),
            (Word, 10) => Register::Word(Register16::R10W),
            (Word, 11) => Register::Word(Register16::R11W),
            (Word, 12) => Register::Word(Register16::R12W),
            (Word, 13) => Register::Word(Register16::R13W),
            (Word, 14) => Register::Word(Register16::R14W),
            (Word, 15) => Register::Word(Register16::R15W),

            (DWord, 0) => Register::DWord(Register32::EAX),
            (DWord, 1) => Register::DWord(Register32::ECX),
            (DWord, 2) => Register::DWord(Register32::EDX),
            (DWord, 3) => Register::DWord(Register32::EBX),
            (DWord, 4) => Register::DWord(Register32::ESP),
            (DWord, 5) => Register::DWord(Register32::EBP),
            (DWord, 6) => Register::DWord(Register32::ESI),
            (DWord, 7) => Register::DWord(Register32::EDI),
            (DWord, 8) => Register::DWord(Register32::R8D),
            (DWord, 9) => Register::DWord(Register32::R9D),
            (DWord, 10) => Register::DWord(Register32::R10D),
            (DWord, 11) => Register::DWord(Register32::R11D),
            (DWord, 12) => Register::DWord(Register32::R12D),
            (DWord, 13) => Register::DWord(Register32::R13D),
            (DWord, 14) => Register::DWord(Register32::R14D),
            (DWord, 15) => Register::DWord(Register32::R15D),

            (QWord, 0) => Register::QWord(Register64::RAX),
            (QWord, 1) => Register::QWord(Register64::RCX),
            (QWord, 2) => Register::QWord(Register64::RDX),
            (QWord, 3) => Register::QWord(Register64::RBX),
            (QWord, 4) => Register::QWord(Register64::RSP),
            (QWord, 5) => Register::QWord(Register64::RBP),
            (QWord, 6) => Register::QWord(Register64::RSI),
            (QWord, 7) => Register::QWord(Register64::RDI),
            (QWord, 8) => Register::QWord(Register64::R8),
            (QWord, 9) => Register::QWord(Register64::R9),
            (QWord, 10) => Register::QWord(Register64::R10),
            (QWord, 11) => Register::QWord(Register64::R11),
            (QWord, 12) => Register::QWord(Register64::R12),
            (QWord, 13) => Register::QWord(Register64::R13),
            (QWord, 14) => Register::QWord(Register64::R14),
            (QWord, 15) => Register::QWord(Register64::R15),

            // FIXME: Probably should not panic, and throw some kind of parsing error.
            (_, _) => panic!("bad register encoding"),
        }
    }
}

crate mod ctors {
    use super::{Register, Register::*, Register32::*, Register64::*, Register8::*};

    crate fn al() -> Register {
        Byte(AL)
    }

    crate fn bl() -> Register {
        Byte(BL)
    }

    crate fn cl() -> Register {
        Byte(CL)
    }

    crate fn dl() -> Register {
        Byte(DL)
    }

    crate fn ah() -> Register {
        Byte(AH)
    }

    crate fn bh() -> Register {
        Byte(BH)
    }

    crate fn ch() -> Register {
        Byte(CH)
    }

    crate fn dh() -> Register {
        Byte(DH)
    }

    crate fn sil() -> Register {
        Byte(SIL)
    }

    crate fn r14l() -> Register {
        Byte(R14L)
    }

    crate fn eax() -> Register {
        DWord(EAX)
    }

    crate fn ebx() -> Register {
        DWord(EBX)
    }

    crate fn ecx() -> Register {
        DWord(ECX)
    }

    crate fn edx() -> Register {
        DWord(EDX)
    }

    crate fn esi() -> Register {
        DWord(ESI)
    }

    crate fn edi() -> Register {
        DWord(EDI)
    }

    crate fn esp() -> Register {
        DWord(ESP)
    }

    crate fn ebp() -> Register {
        DWord(EBP)
    }

    crate fn r8d() -> Register {
        DWord(R8D)
    }

    crate fn r9d() -> Register {
        DWord(R9D)
    }

    crate fn r10d() -> Register {
        DWord(R10D)
    }

    crate fn r11d() -> Register {
        DWord(R11D)
    }

    crate fn r12d() -> Register {
        DWord(R12D)
    }

    crate fn r13d() -> Register {
        DWord(R13D)
    }

    crate fn r14d() -> Register {
        DWord(R14D)
    }

    crate fn r15d() -> Register {
        DWord(R15D)
    }

    crate fn rax() -> Register {
        QWord(RAX)
    }

    crate fn rbx() -> Register {
        QWord(RBX)
    }

    crate fn rcx() -> Register {
        QWord(RCX)
    }

    crate fn rdx() -> Register {
        QWord(RDX)
    }

    crate fn rsi() -> Register {
        QWord(RSI)
    }

    crate fn rdi() -> Register {
        QWord(RDI)
    }

    crate fn rsp() -> Register {
        QWord(RSP)
    }

    crate fn rbp() -> Register {
        QWord(RBP)
    }

    crate fn r8() -> Register {
        QWord(R8)
    }

    crate fn r9() -> Register {
        QWord(R9)
    }

    crate fn r10() -> Register {
        QWord(R10)
    }

    crate fn r11() -> Register {
        QWord(R11)
    }

    crate fn r12() -> Register {
        QWord(R12)
    }

    crate fn r13() -> Register {
        QWord(R13)
    }

    crate fn r14() -> Register {
        QWord(R14)
    }

    crate fn r15() -> Register {
        QWord(R15)
    }
}
