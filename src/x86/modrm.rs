//! The Dreaded ModR/M Byte and it's Known Associates
//!

use nom::*;

use crate::x86::{
    instr::{Displacement, EffectiveAddress, Immediate, LogicalAddress, Operand, ScaleValue},
    register::Register,
    Width::{self, *},
};

#[derive(Debug, PartialEq)]
enum RegField {
    Reg(u8),
    RM(u8),
}

impl RegField {
    fn value(&self) -> u8 {
        match self {
            RegField::Reg(v) => *v,
            RegField::RM(v) => *v,
        }
    }
}

/// ModR/M
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
/// The `Mod` field of the ModR/M byte determines the format of the Effective Address.
/// When using 32-Bit addressing:
///
/// > *FIXME: is "indirect addressing" the correct nomenclature?  In any case be more specific about
/// > how an Effective Address is put together.*
///
///  * 0b00,  with two exceptions, means that the `R/M` field is a register for indirect addressing
///  * 0b11 treats the `R/M` field as a register.

#[derive(Debug, PartialEq)]
crate struct ModRM {
    /// The Mod bits encode the class of Effective Address that the R/M bits indicate.
    mod_bits: u8,
    /// The REG bits may either encode a register, _or_ act as additional opcode bytes.
    reg_bits: u8,
    ///
    rm_bits: u8,
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
                mod_bits: take_bits!(u8, 2)
                    >> reg_bits: take_bits!(u8, 3)
                    >> rm_bits: take_bits!(u8, 3)
                    >> sib: cond!(
                        rm_bits == 0b100 && mod_bits != 0b11,
                        do_parse!(byte: take_bits!(u8, 8) >> (SIB { byte }))
                    )
                    >> disp: bytes!(call!(ModRM::parse_displacement, mod_bits, rm_bits))
                    >> (ModRM {
                        mod_bits,
                        reg_bits,
                        rm_bits,
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
        mod_bits: u8,
        rm_bits: u8,
    ) -> IResult<&[u8], Option<Displacement>> {
        match (mod_bits, rm_bits) {
            (0b00, 0b101) => do_parse!(input, dword: le_i32 >> (Some(Displacement::DWord(dword)))),
            (0b10, _) => do_parse!(input, dword: le_i32 >> (Some(Displacement::DWord(dword)))),
            (0b01, _) => do_parse!(input, byte: le_i8 >> (Some(Displacement::Byte(byte)))),
            (_, _) => Ok((input, None)),
        }
    }

    fn reg_operand(&self, field: RegField, width: Width) -> Operand {
        self.rex.map_or(
            Operand::Register(Register::decode(field.value(), width)),
            |rex| {
                let (bits, reg_ext) = match field {
                    RegField::RM(v) => (v, rex.b),
                    RegField::Reg(v) => (v, rex.r),
                };
                Operand::Register(Register::decode(
                    bits + if reg_ext { 0x08 } else { 0x00 },
                    // REX.W indicates that the operand is 64-bits wide.
                    if rex.w { QWord } else { width },
                ))
            },
        )
    }

    /// Return an 8-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `R/M` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    crate fn r_m8(&self) -> Operand {
        match self.mod_bits {
            0b11 => self.reg_operand(RegField::RM(self.rm_bits), Byte),
            _ => self.memory_8(),
        }
    }

    /// Return a 16-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `R/M` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    crate fn r_m16(&self) -> Operand {
        match self.mod_bits {
            0b11 => self.reg_operand(RegField::RM(self.rm_bits), Word),
            _ => self.memory_16(),
        }
    }

    /// Return a 32-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `R/M` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    crate fn r_m32(&self) -> Operand {
        match self.mod_bits {
            0b11 => self.reg_operand(RegField::RM(self.rm_bits), DWord),
            _ => self.memory_32(),
        }
    }

    /// Return a 64-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `R/M` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    crate fn r_m64(&self) -> Operand {
        match self.mod_bits {
            0b11 => self.reg_operand(RegField::RM(self.rm_bits), QWord),
            _ => self.memory_64(),
        }
    }

    /// Return an 8-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `REG` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    crate fn r_8(&self) -> Operand {
        self.reg_operand(RegField::Reg(self.reg_bits), Byte)
    }

    /// Return a 16-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `REG` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    crate fn r_16(&self) -> Operand {
        self.reg_operand(RegField::Reg(self.reg_bits), Word)
    }

    /// Return a 32-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `REG` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    crate fn r_32(&self) -> Operand {
        self.reg_operand(RegField::Reg(self.reg_bits), DWord)
    }

    /// Return a 64-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `REG` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    crate fn r_64(&self) -> Operand {
        self.reg_operand(RegField::Reg(self.reg_bits), QWord)
    }

    fn memory_8(&self) -> Operand {
        match self.sib {
            Some(_) => self.memory_with_sib(Byte),
            None => {
                match (self.mod_bits, self.rm_bits) {
                    (0b00, 0b101) => Operand::Memory(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
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
            Some(_) => self.memory_with_sib(Word),
            None => {
                match (self.mod_bits, self.rm_bits) {
                    (0b00, 0b101) => Operand::Memory(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
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
            Some(_) => self.memory_with_sib(DWord),
            None => {
                match (self.mod_bits, self.rm_bits) {
                    (0b00, 0b101) => Operand::Memory(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: self.disp,
                        },
                    }),
                    // FIXME
                    (_, _) => Operand::Immediate(Immediate::DWord(0)),
                }
            }
        }
    }

    fn memory_64(&self) -> Operand {
        match self.sib {
            Some(_) => self.memory_with_sib(QWord),
            None => {
                match (self.mod_bits, self.rm_bits) {
                    (0b00, 0b101) => Operand::Memory(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: self.disp,
                        },
                    }),
                    // FIXME
                    (_, _) => Operand::Immediate(Immediate::DWord(0)),
                }
            }
        }
    }

    fn memory_with_sib(&self, width: Width) -> Operand {
        let sib = self.sib.as_ref().cloned().unwrap();

        let rex = self.rex.unwrap_or_else(|| REX::new(0).unwrap());
        let base = sib.base() + if rex.b { 0x08 } else { 0x0 };
        let index = sib.index() + if rex.x { 0x08 } else { 0x0 };

        Operand::Memory(LogicalAddress {
            // FIXME: deal with prefix segment bytes
            segment: None,
            offset: EffectiveAddress {
                base: Some(Register::decode(base, width)),
                index: Some(Register::decode(index, width)),
                scale: sib.scale(),
                displacement: None,
            },
        })
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::register::ctors::*;

    #[test]
    fn modrm() {
        assert_eq!(
            ModRM::new(b"\xe5", None),
            Ok((
                &b""[..],
                ModRM {
                    mod_bits: 0b11,
                    reg_bits: 0b100,
                    rm_bits: 0b101,
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
                    mod_bits: 0b00,
                    reg_bits: 0b010,
                    rm_bits: 0b100,
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
                    mod_bits: 0b01,
                    reg_bits: 0b010,
                    rm_bits: 0b001,
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
                    base: Some(r12()),
                    index: Some(rbx()),
                    scale: Some(ScaleValue::Eight),
                    displacement: None
                }
            })
        );

        // Check RIP addressing
        let (rest, modrm) = ModRM::new(b"\x25\x72\x0a\x20\x00", None).unwrap();
        assert_eq!(
            modrm.r_m32(),
            Operand::Memory(LogicalAddress {
                segment: None,
                offset: EffectiveAddress {
                    base: None,
                    index: None,
                    scale: None,
                    displacement: Some(Displacement::DWord(0x200a72))
                }
            })
        );
    }
}
