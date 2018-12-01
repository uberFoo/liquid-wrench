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
    /// Not available in 64-bit mode.
    SIL,
    /// Not available in 64-bit mode.
    DIL,
    /// Not available in 64-bit mode.
    SPL,
    /// Not available in 64-bit mode.
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
crate enum RegisterWidth {
    Byte,
    Word,
    DWord,
    QWord,
}

// #[derive(Debug, PartialEq)]
// crate struct Register {
//     register:
// }

#[derive(Debug, PartialEq)]
crate enum Register {
    Byte(Register8),
    Word(Register16),
    DWord(Register32),
    QWord(Register64),
    IP,
}

impl Register {
    crate fn decode(b: u8, width: RegisterWidth) -> Self {
        match (width, b) {
            (RegisterWidth::Byte, 0) => Register::Byte(Register8::AL),
            (RegisterWidth::Byte, 1) => Register::Byte(Register8::CL),
            (RegisterWidth::Byte, 2) => Register::Byte(Register8::DL),
            (RegisterWidth::Byte, 3) => Register::Byte(Register8::BL),
            (RegisterWidth::Byte, 4) => Register::Byte(Register8::AH),
            (RegisterWidth::Byte, 5) => Register::Byte(Register8::CH),
            (RegisterWidth::Byte, 6) => Register::Byte(Register8::DH),
            (RegisterWidth::Byte, 7) => Register::Byte(Register8::BH),
            (RegisterWidth::Byte, 8) => Register::Byte(Register8::R8L),
            (RegisterWidth::Byte, 9) => Register::Byte(Register8::R9L),
            (RegisterWidth::Byte, 10) => Register::Byte(Register8::R10L),
            (RegisterWidth::Byte, 11) => Register::Byte(Register8::R11L),
            (RegisterWidth::Byte, 12) => Register::Byte(Register8::R12L),
            (RegisterWidth::Byte, 13) => Register::Byte(Register8::R13L),
            (RegisterWidth::Byte, 14) => Register::Byte(Register8::R14L),
            (RegisterWidth::Byte, 15) => Register::Byte(Register8::R15L),

            (RegisterWidth::Word, 0) => Register::Word(Register16::AX),
            (RegisterWidth::Word, 1) => Register::Word(Register16::CX),
            (RegisterWidth::Word, 2) => Register::Word(Register16::DX),
            (RegisterWidth::Word, 3) => Register::Word(Register16::BX),
            (RegisterWidth::Word, 4) => Register::Word(Register16::SP),
            (RegisterWidth::Word, 5) => Register::Word(Register16::BP),
            (RegisterWidth::Word, 6) => Register::Word(Register16::SI),
            (RegisterWidth::Word, 7) => Register::Word(Register16::DI),
            (RegisterWidth::Word, 8) => Register::Word(Register16::R8W),
            (RegisterWidth::Word, 9) => Register::Word(Register16::R9W),
            (RegisterWidth::Word, 10) => Register::Word(Register16::R10W),
            (RegisterWidth::Word, 11) => Register::Word(Register16::R11W),
            (RegisterWidth::Word, 12) => Register::Word(Register16::R12W),
            (RegisterWidth::Word, 13) => Register::Word(Register16::R13W),
            (RegisterWidth::Word, 14) => Register::Word(Register16::R14W),
            (RegisterWidth::Word, 15) => Register::Word(Register16::R15W),

            (RegisterWidth::DWord, 0) => Register::DWord(Register32::EAX),
            (RegisterWidth::DWord, 1) => Register::DWord(Register32::ECX),
            (RegisterWidth::DWord, 2) => Register::DWord(Register32::EDX),
            (RegisterWidth::DWord, 3) => Register::DWord(Register32::EBX),
            (RegisterWidth::DWord, 4) => Register::DWord(Register32::ESP),
            (RegisterWidth::DWord, 5) => Register::DWord(Register32::EBP),
            (RegisterWidth::DWord, 6) => Register::DWord(Register32::ESI),
            (RegisterWidth::DWord, 7) => Register::DWord(Register32::EDI),
            (RegisterWidth::DWord, 8) => Register::DWord(Register32::R8D),
            (RegisterWidth::DWord, 9) => Register::DWord(Register32::R9D),
            (RegisterWidth::DWord, 10) => Register::DWord(Register32::R10D),
            (RegisterWidth::DWord, 11) => Register::DWord(Register32::R11D),
            (RegisterWidth::DWord, 12) => Register::DWord(Register32::R12D),
            (RegisterWidth::DWord, 13) => Register::DWord(Register32::R13D),
            (RegisterWidth::DWord, 14) => Register::DWord(Register32::R14D),
            (RegisterWidth::DWord, 15) => Register::DWord(Register32::R15D),

            (RegisterWidth::QWord, 0) => Register::QWord(Register64::RAX),
            (RegisterWidth::QWord, 1) => Register::QWord(Register64::RCX),
            (RegisterWidth::QWord, 2) => Register::QWord(Register64::RDX),
            (RegisterWidth::QWord, 3) => Register::QWord(Register64::RBX),
            (RegisterWidth::QWord, 4) => Register::QWord(Register64::RSP),
            (RegisterWidth::QWord, 5) => Register::QWord(Register64::RBP),
            (RegisterWidth::QWord, 6) => Register::QWord(Register64::RSI),
            (RegisterWidth::QWord, 7) => Register::QWord(Register64::RDI),
            (RegisterWidth::QWord, 8) => Register::QWord(Register64::R8),
            (RegisterWidth::QWord, 9) => Register::QWord(Register64::R9),
            (RegisterWidth::QWord, 10) => Register::QWord(Register64::R10),
            (RegisterWidth::QWord, 11) => Register::QWord(Register64::R11),
            (RegisterWidth::QWord, 12) => Register::QWord(Register64::R12),
            (RegisterWidth::QWord, 13) => Register::QWord(Register64::R13),
            (RegisterWidth::QWord, 14) => Register::QWord(Register64::R14),
            (RegisterWidth::QWord, 15) => Register::QWord(Register64::R15),

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
