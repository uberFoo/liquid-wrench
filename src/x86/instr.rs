#[allow(unused_imports)]
use failure::{err_msg, format_err, Error, Fail};
use nom::*;
use num::{Signed, ToPrimitive, Unsigned};

crate mod and;
crate mod call;
crate mod lea;
crate mod mov;
crate mod pop;
crate mod push;
crate mod ret;
crate mod xor;

use self::{and::And, call::Call, lea::Lea, mov::Mov, pop::Pop, push::Push, ret::Ret, xor::Xor};
use crate::x86::{modrm::REX, register::*, Width};

crate trait DecodeInstruction {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction>;
}

pub struct InstructionDecoder<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> InstructionDecoder<'a> {
    pub fn advance(&mut self, len: usize) -> Option<&[u8]> {
        let bytes = self.bytes.get(self.offset..self.offset + len);
        self.offset += len;
        bytes
    }
}

impl<'a> InstructionDecoder<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        InstructionDecoder { bytes, offset: 0 }
    }
}

impl<'a> Iterator for InstructionDecoder<'a> {
    type Item = InstructionWithBytes<'a>;

    #[allow(clippy::cyclomatic_complexity)]
    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.bytes.len() {
            return None;
        };

        // Check for a REX byte, and if found pass it along to the instruction parser.
        // The `unwrap` is ok here because `opt!` will not error.  Also note that the REX bit is
        // wrapped in an `Option` when used going forward.
        let (rest, rex) = opt!(
            &self.bytes[self.offset..],
            bits!(do_parse!(
                tag_bits!(u8, 4, 0x4)
                    >> rex_bits: take_bits!(u8, 4)
                    >> rex: expr_opt!(REX::new(rex_bits))
                    >> (rex)
            ))
        )
        .unwrap();

        let instr = alt!(
            rest,
            apply!(And::try_parse, rex)
                | apply!(Call::try_parse, rex)
                | apply!(Lea::try_parse, rex)
                | apply!(Mov::try_parse, rex)
                | apply!(Pop::try_parse, rex)
                | apply!(Push::try_parse, rex)
                | apply!(Ret::try_parse, rex)
                | apply!(Xor::try_parse, rex)
        );

        match instr {
            Ok((rest, instr)) => {
                let length = self.bytes.len() - self.offset - rest.len();
                let instr = Some(InstructionWithBytes {
                    instr,
                    bytes: &self.bytes[self.offset..self.offset + length],
                });
                self.offset += length;
                instr
                // if let Some(bytes) = self.advance(self.offset - rest.len()) {
                //     Some(InstructionWithBytes { instr, bytes })
                // } else {
                //     None
                // }
            }
            Err(_) => None,
        }
    }
}

/// Instruction that includes Bytes
///
/// Note that this exists because it's the most straightforward path to including the bytes, given
/// the structure of the parsers.
///
/// FIXME: This may be the thing that is exposed higher up.   If not, figure out how to hide it from
/// extra-crate users; it's required to be public because of the [InstructionDecoder] iterator.
#[derive(Debug, PartialEq)]
pub struct InstructionWithBytes<'a> {
    instr: Instruction,
    bytes: &'a [u8],
}

/// An x86-specific instruction
///
/// FIXME: This needs to tie in with an `Instruction` at a higher-level.  Something that is
/// associated with an address, affected registers, and whatever else we need to leverage other
/// tools, and analyses.  Additionally, it needs to work in a streaming-type environment, which
/// implies an iterator.
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
                | apply!(Call::try_parse, rex)
                | apply!(Lea::try_parse, rex)
                | apply!(Mov::try_parse, rex)
                | apply!(Pop::try_parse, rex)
                | apply!(Push::try_parse, rex)
                | apply!(Ret::try_parse, rex)
                | apply!(Xor::try_parse, rex)
        )
    }
}

/// All of the X86 Instructions
///
/// FIXME: Opcode is the wrong name for this.
#[derive(Debug, PartialEq)]
#[non_exhaustive]
crate enum Opcode {
    And,
    Call,
    Lea,
    Mov,
    Pop,
    Push,
    Ret,
    Xor,
}

/// Operand Encoding
///
/// FIXME: The width of the operand is all wrapped up in the values of it's constituents. It may
/// make mare sense to put the width here directly.  That may then open the possibility of using a
/// builder pattern. For instance the ModRM code could create an Operand with a given width, using
/// functions called by the instruction parsing code, in addition to REX info.  The operand could
/// then be passed to some Register code, which would stuff it with the correct register info, based
/// on the width, and the reg bits from ModRM/instruction parser.
#[allow(dead_code)]
#[derive(Debug, PartialEq)]
crate enum Operand {
    Immediate(Immediate),
    Memory(LogicalAddress),
    Port,
    Register(Register),
}

/// Immediate Operands
///
#[allow(dead_code)]
#[derive(Debug, PartialEq)]
crate enum Immediate {
    Byte(i8),
    DWord(i32),
    UByte(u8),
    UDWord(u32),
}

crate struct ImmediateBuilder {
    width: Width,
}

impl ImmediateBuilder {
    crate fn new(w: Width) -> Self {
        ImmediateBuilder { width: w }
    }

    crate fn signed<T: Signed + ToPrimitive>(&self, i: T) -> Operand {
        match self.width {
            Width::Byte => Operand::Immediate(Immediate::Byte(i.to_i8().unwrap())),
            Width::DWord => Operand::Immediate(Immediate::DWord(i.to_i32().unwrap())),
            _ => unreachable!(),
        }
    }

    crate fn unsigned<T: Unsigned + ToPrimitive>(&self, i: T) -> Operand {
        match self.width {
            Width::Byte => Operand::Immediate(Immediate::UByte(i.to_u8().unwrap())),
            Width::DWord => Operand::Immediate(Immediate::UDWord(i.to_u32().unwrap())),
            _ => unreachable!(),
        }
    }
}

impl Immediate {
    crate fn imm8(i: i8) -> Operand {
        Operand::Immediate(Immediate::Byte(i))
    }

    crate fn imm32(i: i32) -> Operand {
        Operand::Immediate(Immediate::DWord(i))
    }
}

#[derive(Debug, PartialEq)]
crate struct LogicalAddress {
    crate segment: Option<Register>,
    crate offset: EffectiveAddress,
}

#[derive(Debug, PartialEq)]
crate struct EffectiveAddress {
    crate base: Option<Register>,
    crate index: Option<Register>,
    crate scale: Option<ScaleValue>,
    crate displacement: Option<Displacement>,
}

#[derive(Debug, PartialEq)]
crate enum ScaleValue {
    Two = 2,
    Four = 4,
    Eight = 8,
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq)]
crate enum Displacement {
    Byte(i8),
    Word(i16),
    DWord(i32),
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::register::ctors::*;

    #[test]
    fn experiment() {
        let test = [0x41, 0x55, 0xc3, 0x58, 0x54, 0xc3];

        let mut decoder = InstructionDecoder::new(&test);

        assert_eq!(
            decoder.next(),
            Some(InstructionWithBytes {
                instr: Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(Operand::Register(r13())),
                    op_2: None,
                    op_3: None,
                },
                bytes: &[0x41, 0x55]
            }),
            "push %r13"
        );

        assert_eq!(
            decoder.next(),
            Some(InstructionWithBytes {
                instr: Instruction {
                    opcode: Opcode::Ret,
                    op_1: None,
                    op_2: None,
                    op_3: None,
                },
                bytes: &[0xc3]
            }),
            "ret"
        );

        assert_eq!(
            decoder.next(),
            Some(InstructionWithBytes {
                instr: Instruction {
                    opcode: Opcode::Pop,
                    op_1: Some(Operand::Register(rax())),
                    op_2: None,
                    op_3: None,
                },
                bytes: &[0x58]
            }),
            "pop %rax"
        );

        assert_eq!(
            decoder.next(),
            Some(InstructionWithBytes {
                instr: Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(Operand::Register(rsp())),
                    op_2: None,
                    op_3: None,
                },
                bytes: &[0x54]
            }),
            "push %rsp"
        );

        assert_eq!(
            decoder.next(),
            Some(InstructionWithBytes {
                instr: Instruction {
                    opcode: Opcode::Ret,
                    op_1: None,
                    op_2: None,
                    op_3: None,
                },
                bytes: &[0xc3]
            }),
            "ret"
        );

        assert_eq!(decoder.next(), None);
    }

    #[test]
    fn one_byte_instrs() {
        let test = b"\x58\x54\xc3";

        let (remainder, result) = Instruction::try_parse(test).unwrap();
        assert_eq!(
            result,
            Instruction {
                opcode: Opcode::Pop,
                op_1: Some(Operand::Register(rax())),
                op_2: None,
                op_3: None,
                // bytes: &[test[0]]
            },
            "pop %rax"
        );

        let (remainder, result) = Instruction::try_parse(remainder).unwrap();
        assert_eq!(
            result,
            Instruction {
                opcode: Opcode::Push,
                op_1: Some(Operand::Register(rsp())),
                op_2: None,
                op_3: None,
                // bytes: &[test[1]]
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
                op_3: None,
                // bytes: &[test[0]]
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
                op_1: Some(Operand::Register(r13())),
                op_2: None,
                op_3: None,
                // bytes: &test[0..3]
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
                op_3: None,
                // bytes: &[test[3]]
            },
            "ret"
        );

        assert_eq!(remainder, &b""[..]);
    }
}
