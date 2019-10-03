//! The Dreaded ModR/M Byte and it's Known Associates
//!

use nom::*;

use crate::x86::{
    instr::{Displacement, EffectiveAddress, LogicalAddress, Operand, ScaleValue},
    register::{
        ctors::{rbp, rip},
        Register,
    },
    Width::{self, *},
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
/// The `Mod` field of the ModR/M byte determines the format of the Effective Address.
/// When using 32-Bit addressing:
///
/// > *FIXME: is "indirect addressing" the correct nomenclature?  In any case be more specific about
/// > how an Effective Address is put together.*
///
///  * 0b00,  with two exceptions, means that the `R/M` field is a register for indirect addressing
///  * 0b11 treats the `R/M` field as a register.

#[derive(Debug, PartialEq)]
pub(crate) struct ModRM {
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
    #[allow(clippy::cognitive_complexity, clippy::useless_let_if_seq)]
    pub(crate) fn new(input: &[u8], rex: Option<REX>) -> IResult<&[u8], ModRM> {
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
                    >> disp: bytes!(call!(ModRM::parse_displacement, mod_bits, rm_bits, sib))
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
        sib: Option<SIB>,
    ) -> IResult<&[u8], Option<Displacement>> {
        if let Some(sib) = sib {
            if sib.base() == 0b101 && mod_bits == 0b00 {
                do_parse!(input, dword: le_i32 >> (Some(Displacement::DWord(dword))))
            } else {
                ModRM::parse_mod_only_displacement(input, mod_bits, rm_bits)
            }
        } else {
            ModRM::parse_mod_only_displacement(input, mod_bits, rm_bits)
        }
    }

    fn parse_mod_only_displacement(
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

    /// Return an 8-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `R/M` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    pub(crate) fn r_m8(&self) -> Operand {
        match self.mod_bits {
            0b11 => Operand::Register(Register::rb(self.rm_bits, self.rex)),
            _ => self.memory(),
        }
    }

    /// Return a 16-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `R/M` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    #[allow(dead_code)]
    pub(crate) fn r_m16(&self) -> Operand {
        match self.mod_bits {
            0b11 => Operand::Register(Register::rw(self.rm_bits, self.rex)),
            _ => self.memory(),
        }
    }

    /// Return a 32-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `R/M` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    pub(crate) fn r_m32(&self) -> Operand {
        match self.mod_bits {
            0b11 => Operand::Register(Register::rd(self.rm_bits, self.rex)),
            _ => self.memory(),
        }
    }

    /// Return a 64-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `R/M` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    #[allow(dead_code)]
    pub(crate) fn r_m64(&self) -> Operand {
        match self.mod_bits {
            0b11 => Operand::Register(Register::ro(self.rm_bits, self.rex)),
            _ => self.memory(),
        }
    }

    /// Return a 128-bit Memory, or Register Operand
    ///
    /// The Operand is based on the `R/M` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    #[allow(dead_code)]
    pub(crate) fn xmm_m128(&self) -> Operand {
        match self.mod_bits {
            0b11 => Operand::Register(Register::sse(self.rm_bits, self.rex)),
            _ => self.memory(),
        }
    }

    /// Return an 8-bit Register Operand
    ///
    /// The Operand is based on the `REG` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    pub(crate) fn r8(&self) -> Operand {
        Operand::Register(Register::r8(self.reg_bits, self.rex))
    }

    /// Return a 16-bit Register Operand
    ///
    /// The Operand is based on the `REG` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    #[allow(dead_code)]
    pub(crate) fn r16(&self) -> Operand {
        Operand::Register(Register::r16(self.reg_bits, self.rex))
    }

    /// Return a 32-bit Register Operand
    ///
    /// The Operand is based on the `REG` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    pub(crate) fn r32(&self) -> Operand {
        Operand::Register(Register::r32(self.reg_bits, self.rex))
    }

    /// Return a 64-bit Register Operand
    ///
    /// The Operand is based on the `REG` field of the ModR/M Byte.
    ///
    /// *Note that a REX byte may indicate that the operand is 64-bits wide.*
    pub(crate) fn r64(&self) -> Operand {
        Operand::Register(Register::r64(self.reg_bits, self.rex))
    }

    /// Return an SSE, 128-bit Register Operand
    ///
    /// The Operand is based on the `REG` field of the ModR/M Byte
    ///
    /// *Node that XMM8 through XMM15 are available using REX.R in 64-bit mode
    pub(crate) fn xmm(&self) -> Operand {
        Operand::Register(Register::xmm(self.reg_bits, self.rex))
    }

    /// Return a Memory Operand
    ///
    /// Since we only do 64-bit (IA-32e), we default to 64-bit registers.
    fn memory(&self) -> Operand {
        match self.sib {
            Some(_) => self.memory_with_sib(Byte),
            None => {
                match (self.mod_bits, self.rm_bits) {
                    // 32-bit displacement / RIP
                    (0b00, 0b101) => Operand::Memory(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rip()),
                            index: None,
                            scale: None,
                            displacement: self.disp,
                        },
                    }),
                    (_, reg) => Operand::Memory(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(Register::ro(reg, self.rex)),
                            index: None,
                            scale: None,
                            displacement: self.disp,
                        },
                    }),
                }
            }
        }
    }

    /// Use the SIB Byte to Calculate an Effective Address
    ///
    /// *FIXME: This currently defaults to using 64-bit registers for the base and index.*
    /// *FIXME: Missing prefix segment.*
    fn memory_with_sib(&self, _width: Width) -> Operand {
        let sib = self.sib.as_ref().cloned().unwrap();

        if sib.base() == 0b101 {
            if self.mod_bits == 0b00 {
                Operand::Memory(LogicalAddress {
                    segment: None,
                    offset: EffectiveAddress {
                        base: None,
                        index: Some(Register::r64(sib.index(), self.rex)),
                        scale: sib.scale(),
                        displacement: self.disp,
                    },
                })
            } else {
                Operand::Memory(LogicalAddress {
                    segment: None,
                    offset: EffectiveAddress {
                        base: Some(rbp()),
                        index: Some(Register::r64(sib.index(), self.rex)),
                        scale: sib.scale(),
                        displacement: self.disp,
                    },
                })
            }
        } else if self.rm_bits == 0b100 && sib.index() == 0b100 {
            Operand::Memory(LogicalAddress {
                segment: None,
                offset: EffectiveAddress {
                    base: Some(Register::ro(sib.base(), self.rex)),
                    index: None,
                    scale: None,
                    displacement: self.disp,
                },
            })
        } else {
            Operand::Memory(LogicalAddress {
                segment: None,
                offset: EffectiveAddress {
                    base: Some(Register::ro(sib.base(), self.rex)),
                    index: Some(Register::r64(sib.index(), self.rex)),
                    scale: sib.scale(),
                    displacement: None,
                },
            })
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) struct REX {
    pub w: bool,
    pub r: bool,
    pub x: bool,
    pub b: bool,
}

impl REX {
    pub(crate) fn new(bits: u8) -> Option<Self> {
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
pub(crate) struct SIB {
    byte: u8,
}

impl SIB {
    pub(crate) fn scale(self) -> Option<ScaleValue> {
        match self.byte >> 6 {
            0b00 => None,
            0b01 => Some(ScaleValue::Two),
            0b10 => Some(ScaleValue::Four),
            0b11 => Some(ScaleValue::Eight),
            _ => panic!("SIB scale -- I don't think this can happen."),
        }
    }

    pub(crate) fn index(self) -> u8 {
        self.byte >> 3 & 0x07
    }

    pub(crate) fn base(self) -> u8 {
        self.byte & 0x07
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::register::ctors::*;

    // FIXME:
    // * Test all the SIB/displacement combinations.
    // * Test the following from the reference:
    //    The ModR/M encoding for RIP-relative addressing does not depend on using a prefix.
    //    Specifically, the r/m bit field encoding of 101B (used to select RIP-relative addressing)
    //    is not affected by the REX prefix. For example, selecting R13 (REX.B = 1, r/m = 101B) with
    //    mod = 00B still results in RIP-relative addressing. The 4-bit r/m field of REX.B combined
    //    with ModR/M is not fully decoded. In order to address R13 with no displacement, software
    //    must encode R13 + 0 using a 1-byte displacement of zero.
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

        // Check 32-bit memory, no SIB, no REX
        // This is maybe silly, since we're in 64-bit addressing mode.
        let (_, modrm) = ModRM::new(b"\x46\x68", None).unwrap();
        assert_eq!(
            modrm.r_m32(),
            Operand::Memory(LogicalAddress {
                segment: None,
                offset: EffectiveAddress {
                    base: Some(rsi()),
                    index: None,
                    scale: None,
                    displacement: Some(Displacement::Byte(104_i8))
                }
            })
        );

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
        assert_eq!(rest, &b""[..]);
        assert_eq!(
            modrm.r_m32(),
            Operand::Memory(LogicalAddress {
                segment: None,
                offset: EffectiveAddress {
                    base: Some(rip()),
                    index: None,
                    scale: None,
                    displacement: Some(Displacement::DWord(0x20_0a72))
                }
            })
        );
    }
}
