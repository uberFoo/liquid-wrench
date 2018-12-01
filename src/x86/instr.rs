use failure::{err_msg, format_err, Error, Fail};
use nom::*;

crate mod and;
crate mod mov;
crate mod pop;
crate mod push;
crate mod ret;
crate mod xor;

use self::{and::And, mov::Mov, pop::Pop, push::Push, ret::Ret, xor::Xor};
use crate::x86::*;

crate trait DecodeInstruction {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction>;
}

/// An x86-specific instruction
///
#[derive(Debug, PartialEq)]
pub struct Instruction {
    /// The [Opcode].
    opcode: Opcode,
    /// The first operand, typically the "destination".
    op_1: Option<Operand>,
    /// The second operand, typically the "source".
    op_2: Option<Operand>,
    /// The third operand, often an "immediate" value.
    op_3: Option<Operand>,
}

impl Instruction {
    #[allow(clippy::cyclomatic_complexity)]
    crate fn try_parse(input: &[u8]) -> IResult<&[u8], Self> {
        // Check for a REX byte, and if found pass it along to the instruction parser.
        let (input, rex) = opt!(
            input,
            bits!(do_parse!(
                tag_bits!(u8, 4, 0x4)
                    >> rex_bits: take_bits!(u8, 4)
                    >> rex: expr_opt!(REX::new(rex_bits))
                    >> (rex)
            ))
        )
        .unwrap();

        alt!(
            input,
            apply!(And::try_parse, rex)
                | apply!(Mov::try_parse, rex)
                | apply!(Pop::try_parse, rex)
                | apply!(Push::try_parse, rex)
                | apply!(Ret::try_parse, rex)
                | apply!(Xor::try_parse, rex)
        )
    }
}

#[derive(Debug, PartialEq)]
#[non_exhaustive]
crate enum Opcode {
    And,
    Mov,
    Pop,
    Push,
    Ret,
    Xor,
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

#[derive(Debug, PartialEq)]
crate enum Operand {
    Immediate(Immediate),
    Memory(LogicalAddress),
    Port,
    Register(Register),
}

/// Immediate Operands
///
#[derive(Debug, PartialEq)]
crate enum Immediate {
    Byte(i8),
    DWord(i32),
    UByte(u8),
    UDWord(u32),
}

#[derive(Debug, PartialEq)]
crate struct LogicalAddress {
    segment: Option<Register>,
    offset: EffectiveAddress,
}

#[derive(Debug, PartialEq)]
crate struct EffectiveAddress {
    base: Register,
    index: Option<Register>,
    scale: Option<ScaleValue>,
    displacement: Option<Displacement>,
}

#[derive(Debug, PartialEq)]
crate enum ScaleValue {
    Two = 2,
    Four = 4,
    Eight = 8,
}

#[derive(Copy, Clone, Debug, PartialEq)]
crate enum Displacement {
    Byte(i8),
    DWord(i32),
}

#[derive(Copy, Clone, Debug, PartialEq)]
crate struct REX {
    pub w: bool,
    pub r: bool,
    pub x: bool,
    pub b: bool,
}

impl REX {
    crate fn new(bits: u8) -> Option<Self> {
        Some(REX {
            w: bits >> 3 & 0x01 == 1,
            r: bits >> 2 & 0x01 == 1,
            x: bits >> 1 & 0x01 == 1,
            b: bits & 0x01 == 1,
        })
    }
}

/// The SIB Byte
///
/// The SIB byte is used in conjunction with the [ModRM] byte to "extend" the range of operand
/// effective addresses.
///
/// This is coded entirely differently than the ModRM byte, and I suppose I did it this way because
/// I'm torn about what's best, so I figure one of each for now, and maybe that will help be
/// determine the best option?
///
/// FIXME: better explain "scale", "index", and "base".
/// NTS: This had got me thinking about the `packed_struct` crate, and I wonder if it'd be useful?
#[derive(Copy, Clone, Debug, PartialEq)]
crate struct SIB {
    byte: u8,
}

impl SIB {
    crate fn scale(&self) -> Option<ScaleValue> {
        match self.byte >> 6 {
            0b00 => None,
            0b01 => Some(ScaleValue::Two),
            0b10 => Some(ScaleValue::Four),
            0b11 => Some(ScaleValue::Eight),
            _ => panic!("SIB scale -- I don't think this can happen."),
        }
    }

    crate fn index(&self) -> u8 {
        self.byte >> 3 & 0x07
    }

    crate fn base(&self) -> u8 {
        self.byte & 0x07
    }
}

/// The Dreaded ModR/M Byte
///
/// Each bit-field of the byte is split out into it's own u8.  This is not space efficient, and we
/// could instead choose to store the byte, and surface methods to access the fields.
///
/// The optional SIB, REX, and displacement bytes are stored here as well.  It's not only
/// convenient, but this is the only place that they are used.
///
/// Note that the REX byte is before the operand, and thus must be passed to the `new` method.  The
/// other values come after the ModR/M byte, and are parsed as part of the constructor.
///
#[derive(Debug, PartialEq)]
crate struct ModRM {
    /// The Mod bits encode the class of Effective Address that the R/M bits indicate.
    mod_value: u8,
    /// The REG bits may either encode a register, _or_ act as additional opcode bytes.
    reg_value: u8,
    ///
    rm_value: u8,
    rex: Option<REX>,
    sib: Option<SIB>,
    disp: Option<Displacement>,
}

impl ModRM {
    #[allow(clippy::cyclomatic_complexity, clippy::useless_let_if_seq)]
    crate fn new(input: &[u8], rex: Option<REX>) -> IResult<&[u8], ModRM> {
        bits!(
            input,
            do_parse!(
                mod_value: take_bits!(u8, 2)
                    >> reg_value: take_bits!(u8, 3)
                    >> rm_value: take_bits!(u8, 3)
                    >> sib: cond!(
                        rm_value == 0b100 && mod_value != 0b11,
                        do_parse!(byte: take_bits!(u8, 8) >> (SIB { byte }))
                    )
                    >> disp: bytes!(call!(ModRM::parse_displacement, mod_value, rm_value))
                    >> (ModRM {
                        mod_value,
                        reg_value,
                        rm_value,
                        rex,
                        sib,
                        disp,
                    })
            )
        )
    }

    /// Parse displacement Bytes
    ///
    /// Using `alt!(cond!()|cond!())` doesn't want to work...
    #[allow(dead_code)]
    fn parse_displacement(
        input: &[u8],
        mod_value: u8,
        rm_value: u8,
    ) -> IResult<&[u8], Option<Displacement>> {
        match (mod_value, rm_value) {
            (0b00, 0b101) => do_parse!(input, dword: le_i32 >> (Some(Displacement::DWord(dword)))),
            (0b10, _) => do_parse!(input, dword: le_i32 >> (Some(Displacement::DWord(dword)))),
            (0b01, _) => do_parse!(input, byte: le_i8 >> (Some(Displacement::Byte(byte)))),
            (_, _) => Ok((input, None)),
        }
    }

    crate fn r_m8(&self) -> Operand {
        match self.mod_value {
            // 0b11 => self.r_8(),
            0b11 => {
                let rex = self.rex.unwrap_or_else(|| REX::new(0).unwrap());
                let rm = self.rm_value + if rex.b { 0x08 } else { 0x0 };
                if (rex.w) {
                    Operand::Register(Register::decode(rm, RegisterWidth::Byte))
                } else {
                    Operand::Register(Register::decode(rm, RegisterWidth::Byte))
                }
            }
            _ => self.memory_8(),
        }
    }

    crate fn r_m16(&self) -> Operand {
        match self.mod_value {
            // 0b11 => self.r_8(),
            0b11 => {
                let rex = self.rex.unwrap_or_else(|| REX::new(0).unwrap());
                let rm = self.rm_value + if rex.b { 0x08 } else { 0x0 };
                if (rex.w) {
                    Operand::Register(Register::decode(rm, RegisterWidth::Word))
                } else {
                    Operand::Register(Register::decode(rm, RegisterWidth::Word))
                }
            }
            _ => self.memory_16(),
        }
    }

    crate fn r_m32(&self) -> Operand {
        match self.mod_value {
            // 0b11 => self.r_32(),
            0b11 => {
                let rex = self.rex.unwrap_or_else(|| REX::new(0).unwrap());
                let rm = self.rm_value + if rex.b { 0x08 } else { 0x0 };
                if (rex.w) {
                    Operand::Register(Register::decode(rm, RegisterWidth::QWord))
                } else {
                    Operand::Register(Register::decode(rm, RegisterWidth::DWord))
                }
            }
            _ => self.memory_32(),
        }
    }

    crate fn r_m64(&self) -> Operand {
        match self.mod_value {
            // 0b11 => self.r_64(),
            _ => self.memory_64(),
        }
    }

    crate fn r_8(&self) -> Operand {
        let rex = self.rex.unwrap_or_else(|| REX::new(0).unwrap());
        let reg = self.reg_value + if rex.b { 0x08 } else { 0x0 };
        Operand::Register(Register::decode(reg, RegisterWidth::Byte))
    }

    crate fn r_16(&self) -> Operand {
        let rex = self.rex.unwrap_or_else(|| REX::new(0).unwrap());
        let reg = self.reg_value + if rex.b { 0x08 } else { 0x0 };
        Operand::Register(Register::decode(reg, RegisterWidth::Word))
    }

    crate fn r_32(&self) -> Operand {
        let rex = self.rex.unwrap_or_else(|| REX::new(0).unwrap());
        let reg = self.reg_value + if rex.r { 0x08 } else { 0x0 };
        if (rex.w) {
            Operand::Register(Register::decode(reg, RegisterWidth::QWord))
        } else {
            Operand::Register(Register::decode(reg, RegisterWidth::DWord))
        }
    }

    fn r_64(&self) -> Operand {
        let rex = self.rex.unwrap_or_else(|| REX::new(0).unwrap());
        let reg = self.reg_value + if rex.b { 0x08 } else { 0x0 };
        Operand::Register(Register::decode(reg, RegisterWidth::QWord))
    }

    fn memory_8(&self) -> Operand {
        match self.sib {
            Some(_) => self.r_m64_with_sib(),
            None => {
                match (self.mod_value, self.rm_value) {
                    (0b00, 0b101) => Operand::Memory(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Register::IP,
                            index: None,
                            scale: None,
                            displacement: self.disp,
                        },
                    }),
                    // FIXME
                    (_, _) => Operand::Immediate(Immediate::Byte(0)),
                }
            }
        }
    }

    fn memory_16(&self) -> Operand {
        match self.sib {
            Some(_) => self.r_m64_with_sib(),
            None => {
                match (self.mod_value, self.rm_value) {
                    (0b00, 0b101) => Operand::Memory(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Register::IP,
                            index: None,
                            scale: None,
                            displacement: self.disp,
                        },
                    }),
                    // FIXME
                    (_, _) => Operand::Immediate(Immediate::Byte(0)),
                }
            }
        }
    }

    fn memory_32(&self) -> Operand {
        match self.sib {
            Some(_) => self.r_m64_with_sib(),
            None => {
                match (self.mod_value, self.rm_value) {
                    (0b00, 0b101) => Operand::Memory(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Register::IP,
                            index: None,
                            scale: None,
                            displacement: self.disp,
                        },
                    }),
                    // FIXME
                    (_, _) => Operand::Immediate(Immediate::Byte(0)),
                }
            }
        }
    }

    fn memory_64(&self) -> Operand {
        // FIXME: Yukky return...
        if self.sib.is_some() {
            return self.r_m64_with_sib();
        }

        if self.mod_value == 0b00 && self.rm_value == 0b101 {
            Operand::Memory(LogicalAddress {
                segment: None,
                offset: EffectiveAddress {
                    base: Register::IP,
                    index: None,
                    scale: None,
                    displacement: self.disp,
                },
            })
        } else {
            // FIXME
            Operand::Immediate(Immediate::Byte(0))
        }
    }

    fn r_m64_with_sib(&self) -> Operand {
        let sib = self.sib.as_ref().cloned().unwrap();

        let rex = self.rex.unwrap_or_else(|| REX::new(0).unwrap());
        let base = sib.base() + if rex.b { 0x08 } else { 0x0 };
        let mut index = sib.index() + if rex.x { 0x08 } else { 0x0 };

        Operand::Memory(LogicalAddress {
            // FIXME: deal with prefix segment bytes
            segment: None,
            offset: EffectiveAddress {
                base: Register::decode(base, RegisterWidth::QWord),
                index: Some(Register::decode(index, RegisterWidth::QWord)),
                scale: sib.scale(),
                displacement: None,
            },
        })
    }
}

// named_args!(pub(crate)
//     check_opcode_ext(needed: u8 )<bool>,
//     peek!(bits!(do_parse!(
//         take_bits!(u8, 8) >> take_bits!(u8, 2) >> ext_bits: take_bits!(u8, 3) >> (ext_bits)
//     )))
// );

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn modrm() {
        assert_eq!(
            ModRM::new(b"\xe5", None),
            Ok((
                &b""[..],
                ModRM {
                    mod_value: 0b11,
                    reg_value: 0b100,
                    rm_value: 0b101,
                    rex: None,
                    sib: None,
                    disp: None
                }
            ))
        );

        // Check that we load a SIB byte when indicated.
        assert_eq!(
            ModRM::new(b"\x14\xdc", None),
            Ok((
                &b""[..],
                ModRM {
                    mod_value: 0b00,
                    reg_value: 0b010,
                    rm_value: 0b100,
                    rex: None,
                    sib: Some(SIB { byte: 0xdc }),
                    disp: None
                }
            ))
        );

        // Check that we handle an 8-bit displacement
        assert_eq!(
            ModRM::new(b"\x51\xff", None),
            Ok((
                &b""[..],
                ModRM {
                    mod_value: 0b01,
                    reg_value: 0b010,
                    rm_value: 0b001,
                    rex: None,
                    sib: None,
                    disp: Some(Displacement::Byte(-1_i8))
                }
            ))
        );

        // FIXME: Test all the SIB/displacement combinations.

        // Check SIB byte without displacement (with REX)
        let (_, modrm) = ModRM::new(b"\x14\xdc", REX::new(0x41)).unwrap();
        assert_eq!(
            modrm.r_m64(),
            Operand::Memory(LogicalAddress {
                segment: None,
                offset: EffectiveAddress {
                    base: Register::QWord(Register64::R12),
                    index: Some(Register::QWord(Register64::RBX)),
                    scale: Some(ScaleValue::Eight),
                    displacement: None
                }
            })
        );

        // Check RIP addressing
        let (rest, modrm) = ModRM::new(b"\x25\x72\x0a\x20\x00", None).unwrap();
        assert_eq!(
            modrm.r_m64(),
            Operand::Memory(LogicalAddress {
                segment: None,
                offset: EffectiveAddress {
                    base: Register::IP,
                    index: None,
                    scale: None,
                    displacement: Some(Displacement::DWord(0x200a72))
                }
            })
        );
    }

    #[test]
    fn one_byte_instrs() {
        let test = b"\x58\x54\xc3";

        let (remainder, result) = Instruction::try_parse(test).unwrap();
        assert_eq!(
            result,
            Instruction {
                opcode: Opcode::Pop,
                op_1: Some(Operand::Register(Register::QWord(Register64::RAX))),
                op_2: None,
                op_3: None
            },
            "pop %rax"
        );

        let (remainder, result) = Instruction::try_parse(remainder).unwrap();
        assert_eq!(
            result,
            Instruction {
                opcode: Opcode::Push,
                op_1: Some(Operand::Register(Register::QWord(Register64::RSP))),
                op_2: None,
                op_3: None
            },
            "push %rsp"
        );

        let (remainder, result) = Instruction::try_parse(remainder).unwrap();
        assert_eq!(
            result,
            Instruction {
                opcode: Opcode::Ret,
                op_1: None,
                op_2: None,
                op_3: None
            },
            "ret"
        );

        assert_eq!(remainder, &b""[..]);
    }

    #[test]
    fn rex_instr() {
        let test = b"\x41\x55\xc3";

        let (remainder, result) = Instruction::try_parse(test).unwrap();
        assert_eq!(
            result,
            Instruction {
                opcode: Opcode::Push,
                op_1: Some(Operand::Register(Register::QWord(Register64::R13))),
                op_2: None,
                op_3: None
            },
            "push %r13"
        );

        let (remainder, result) = Instruction::try_parse(remainder).unwrap();
        assert_eq!(
            result,
            Instruction {
                opcode: Opcode::Ret,
                op_1: None,
                op_2: None,
                op_3: None
            },
            "ret"
        );

        assert_eq!(remainder, &b""[..]);
    }
}
