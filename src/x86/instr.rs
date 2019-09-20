use std::fmt::{self, Write};

use colored::*;
#[allow(unused_imports)]
use failure::{err_msg, format_err, Error, Fail};
use nom::*;
use num::{Signed, ToPrimitive, Unsigned};

crate mod add;
crate mod and;
crate mod call;
crate mod cmp;
crate mod jcc;
crate mod jmp;
crate mod lea;
crate mod mov;
crate mod pop;
crate mod push;
crate mod ret;
crate mod sub;
crate mod test;
crate mod xor;

use self::{
    add::Add,
    and::And,
    call::Call,
    cmp::Cmp,
    jcc::{Je, Jg, Jge},
    jmp::Jmp,
    lea::Lea,
    mov::Mov,
    pop::Pop,
    push::Push,
    ret::Ret,
    sub::Sub,
    test::Test,
    xor::Xor,
};
use crate::{
    x86::{modrm::REX, register::*, Width},
    ByteSpan,
};

pub(in crate::x86) struct InstructionDecoder<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> InstructionDecoder<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        InstructionDecoder { bytes, offset: 0 }
    }
}

impl<'a> Iterator for InstructionDecoder<'a> {
    type Item = ByteSpan<Instruction>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut i = self.offset;
        while i < self.bytes.len() {
            let instr = Instruction::try_parse(&self.bytes[i..]);
            match instr {
                Ok((rest, instr)) => {
                    // This is sort of funky...
                    // If there were no problems parsing, then we want to return the results of
                    // this instruction.  However, if we ran into some junk bytes, we want to return
                    // all the junk we found _prior_ to this instruction.  The implication being
                    // that we won't return _this_ instruction until the next time.
                    if i == self.offset {
                        let length = self.bytes.len() - i - rest.len();
                        self.offset = i + length;
                        return Some(ByteSpan {
                            interpretation: Some(instr),
                            bytes: (i..i + length),
                        });
                    } else {
                        let j = self.offset;
                        self.offset = i;
                        return Some(ByteSpan {
                            interpretation: None,
                            bytes: (j..i),
                        });
                    }
                }
                Err(_) => {
                    i += 1;
                }
            }
        }

        None
    }
}

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
    #[allow(clippy::cognitive_complexity)]
    crate fn try_parse(input: &[u8]) -> IResult<&[u8], Self> {
        // Check for a REX byte, and if found pass it along to the instruction parser.
        // The `unwrap` is ok here because `opt!` will not error.  Also note that the REX bit is
        // wrapped in an `Option` when used going forward.
        let (input, rex) = opt!(
            input,
            bits!(do_parse!(
                // REX bytes range from 0x40 through 0x4f, so we look for 0x4. If we find it, then
                // we create a new REX with it's constructor.
                tag_bits!(u8, 4, 0x4)
                    >> rex_bits: take_bits!(u8, 4)
                    >> rex: expr_opt!(REX::new(rex_bits))
                    >> (rex)
            ))
        )
        .unwrap();

        alt!(
            input,
            apply!(Add::try_parse, rex)
                | apply!(And::try_parse, rex)
                | apply!(Call::try_parse, rex)
                | apply!(Cmp::try_parse, rex)
                | apply!(Je::try_parse, rex)
                | apply!(Jg::try_parse, rex)
                | apply!(Jge::try_parse, rex)
                | apply!(Jmp::try_parse, rex)
                | apply!(Lea::try_parse, rex)
                | apply!(Mov::try_parse, rex)
                | apply!(Pop::try_parse, rex)
                | apply!(Push::try_parse, rex)
                | apply!(Ret::try_parse, rex)
                | apply!(Sub::try_parse, rex)
                | apply!(Test::try_parse, rex)
                | apply!(Xor::try_parse, rex)
        )
    }
}

impl fmt::Display for Instruction {
    // FIXME: We should figure out how to allow for Intel and AT&T format.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\t", self.opcode).expect("unable to write");

        if let Some(operand) = &self.op_2 {
            write!(f, "{}, ", operand).expect("unable to write");
            if let Some(operand) = &self.op_1 {
                write!(f, "{}", operand)
            } else {
                Ok(())
            }
        } else if let Some(operand) = &self.op_1 {
            write!(f, "{}", operand)
        } else {
            Ok(())
        }
    }
}

/// All of the X86 Instructions
///
/// FIXME: Opcode is the wrong name for this.
#[derive(Debug, PartialEq)]
#[non_exhaustive]
crate enum Opcode {
    Add,
    And,
    Call,
    Cmp,
    Je,
    Jg,
    Jge,
    Jmp,
    Lea,
    Mov,
    Pop,
    Push,
    Ret,
    Sub,
    Test,
    Xor,
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Opcode::Add => "add",
            Opcode::And => "and",
            Opcode::Call => "call",
            Opcode::Cmp => "cmp",
            Opcode::Je => "je",
            Opcode::Jg => "jg",
            Opcode::Jge => "jge",
            Opcode::Jmp => "jmp",
            Opcode::Lea => "lea",
            Opcode::Mov => "mov",
            Opcode::Pop => "pop",
            Opcode::Push => "push",
            Opcode::Ret => "ret",
            Opcode::Sub => "sub",
            Opcode::Test => "test",
            Opcode::Xor => "xor",
        };
        write!(f, "{:>7}", s.green())
    }
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

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Register(r) => write!(f, "%{}", r),
            Operand::Immediate(i) => write!(f, "${}", i),
            Operand::Memory(a) => write!(f, "{}", a),
            _ => write!(f, "fixme"),
        }
    }
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

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // let mut s = String::new();
        // match self {
        //     Immediate::Byte(n) => write!(&mut s, "0x{:x}", n).expect("unable to write"),
        //     Immediate::DWord(n) => write!(&mut s, "0x{:x}", n).expect("unable to write"),
        //     Immediate::UByte(n) => write!(&mut s, "0x{:x}", n).expect("unable to write"),
        //     Immediate::UDWord(n) => write!(&mut s, "0x{:x}", n).expect("unable to write"),
        // };

        let s = match self {
            Immediate::Byte(n) => n.to_string(),
            Immediate::DWord(n) => n.to_string(),
            Immediate::UByte(n) => n.to_string(),
            Immediate::UDWord(n) => n.to_string(),
        };
        write!(f, "{}", s.cyan())
    }
}

#[derive(Debug, PartialEq)]
crate struct LogicalAddress {
    crate segment: Option<Register>,
    crate offset: EffectiveAddress,
}

impl fmt::Display for LogicalAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.offset)
    }
}

#[derive(Debug, PartialEq)]
crate struct EffectiveAddress {
    crate base: Option<Register>,
    crate index: Option<Register>,
    crate scale: Option<ScaleValue>,
    crate displacement: Option<Displacement>,
}

impl fmt::Display for EffectiveAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(displacement) = self.displacement {
            write!(f, "{}", displacement).expect("unable to write");
        }
        if let Some(base) = &self.base {
            write!(f, "(%{})", base).expect("unable to write");
        }
        Ok(())
    }
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

impl fmt::Display for Displacement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        match self {
            Displacement::Byte(n) => write!(&mut s, "0x{:x}", n).expect("unable to write"),
            Displacement::Word(n) => write!(&mut s, "0x{:x}", n).expect("unable to write"),
            Displacement::DWord(n) => write!(&mut s, "0x{:x}", n).expect("unable to write"),
        };
        write!(f, "{}", s.cyan())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::register::ctors::*;

    #[test]
    fn decode_instr_iter() {
        let test = [0xc3, 0x00, 0x00, 0x41, 0x55, 0x58, 0x54, 0xc3];

        let mut decoder = InstructionDecoder::new(&test);

        assert_eq!(
            decoder.next(),
            Some(ByteSpan {
                interpretation: Some(Instruction {
                    opcode: Opcode::Ret,
                    op_1: None,
                    op_2: None,
                    op_3: None,
                }),
                bytes: (0..1)
            }),
            "ret"
        );

        assert_eq!(
            decoder.next(),
            Some(ByteSpan {
                interpretation: None,
                bytes: (1..3)
            }),
            "not an instruction"
        );

        assert_eq!(
            decoder.next(),
            Some(ByteSpan {
                interpretation: Some(Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(Operand::Register(r13())),
                    op_2: None,
                    op_3: None,
                }),
                bytes: (3..5)
            }),
            "push %r13"
        );

        assert_eq!(
            decoder.next(),
            Some(ByteSpan {
                interpretation: Some(Instruction {
                    opcode: Opcode::Pop,
                    op_1: Some(Operand::Register(rax())),
                    op_2: None,
                    op_3: None,
                }),
                bytes: (5..6)
            }),
            "pop %rax"
        );

        assert_eq!(
            decoder.next(),
            Some(ByteSpan {
                interpretation: Some(Instruction {
                    opcode: Opcode::Push,
                    op_1: Some(Operand::Register(rsp())),
                    op_2: None,
                    op_3: None,
                }),
                bytes: (6..7)
            }),
            "push %rsp"
        );

        assert_eq!(
            decoder.next(),
            Some(ByteSpan {
                interpretation: Some(Instruction {
                    opcode: Opcode::Ret,
                    op_1: None,
                    op_2: None,
                    op_3: None,
                }),
                bytes: (7..8)
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
            },
            "ret"
        );

        assert_eq!(remainder, &b""[..]);
    }
}
