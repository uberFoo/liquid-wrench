//! Intel 64-bit (IA-32e) Disassembler
//!
//! NB: This is _experimental_.  It's hardly complete, and does not currently consider all of the
//! operating modes of the processor.
//!
// use failure::{err_msg, format_err, Error, Fail};

mod instr;
mod prefix;

pub use self::instr::Instruction;

use self::instr::{Opcode, Operand};

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

// #[derive(Debug, Fail)]
// enum RegisterParseError {
//     #[fail(display = "unknown register bits: {}", bits)]
//     UnknownRegisterBits { bits: u8 },
// }
