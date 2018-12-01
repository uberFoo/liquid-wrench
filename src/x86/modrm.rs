//! The Dreaded ModR/M Byte and it's Known Associates
//!

use nom::*;

use crate::x86::{
    instr::{Displacement, EffectiveAddress, Immediate, LogicalAddress, Operand, ScaleValue},
    register::{Register, RegisterWidth},
};

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
                    base: r12(),
                    index: Some(rbx()),
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
}
