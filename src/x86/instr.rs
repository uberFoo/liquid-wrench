use {
    colored::*,
    nom::*,
    num::{Signed, ToPrimitive, Unsigned},
    serde::{Deserialize, Serialize},
    std::fmt::{self, Write},
};

pub(crate) mod add;
pub(crate) mod and;
pub(crate) mod call;
pub(crate) mod clc;
pub(crate) mod cmovcc;
pub(crate) mod cmp;
pub(crate) mod dec;
pub(crate) mod inc;
pub(crate) mod jcc;
pub(crate) mod jmp;
pub(crate) mod lea;
pub(crate) mod mov;
pub(crate) mod movsx;
pub(crate) mod movzx;
pub(crate) mod nop;
pub(crate) mod or;
pub(crate) mod pop;
pub(crate) mod push;
pub(crate) mod ret;
pub(crate) mod setcc;
pub(crate) mod shift;
pub(crate) mod sub;
pub(crate) mod test;
pub(crate) mod xor;

use self::{
    add::Add,
    and::And,
    call::Call,
    clc::Clc,
    cmovcc::Cmove,
    cmovcc::Cmovne,
    cmp::Cmp,
    dec::Dec,
    inc::Inc,
    jcc::{Ja, Jae, Je, Jg, Jge, Jl, Jne, Jns},
    jmp::Jmp,
    lea::Lea,
    mov::Mov,
    movsx::Movsx,
    movzx::Movzx,
    nop::Nop,
    or::Or,
    pop::Pop,
    push::Push,
    ret::Ret,
    setcc::{Sete, Setne},
    shift::{Sar, Shl, Shr},
    sub::Sub,
    test::Test,
    xor::Xor,
};

use crate::{
    x86::{
        modrm::REX,
        prefix::{prefixes, Prefix},
        register::*,
        Width,
    },
    ByteSpan,
};

pub(crate) struct InstructionDecoder<'a> {
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

pub(crate) trait DecodeInstruction {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction>;
}

/// An x86-specific instruction
///
#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub struct Instruction {
    /// The [Opcode].
    opcode: Opcode,
    /// The width of the instruction
    width: Width,
    /// The first operand, typically the "destination".
    op_1: Option<Operand>,
    /// The second operand, typically the "source".
    op_2: Option<Operand>,
    /// The third operand, often an "immediate" value.
    op_3: Option<Operand>,
}

impl Instruction {
    #[allow(clippy::cognitive_complexity)]
    pub(crate) fn try_parse(input: &[u8]) -> IResult<&[u8], Self> {
        // Check for a REX byte, and if found pass it along to the instruction parser.
        // The `unwrap` is ok here because `opt!` will not error.  Also note that the REX bit is
        // wrapped in an `Option` when used going forward.
        let (input, prefix) = do_parse!(
            input,
            prefix_bytes: prefixes
                >> rex: opt!(bits!(do_parse!(
                    // REX bytes range from 0x40 through 0x4f, so we look for 0x4. If we find it,
                    // then we create a new REX with it's constructor.
                    tag_bits!(u8, 4, 0x4)
                        >> rex_bits: take_bits!(u8, 4)
                        >> rex: expr_opt!(REX::new(rex_bits))
                        >> (rex)
                )))
                >> (PrefixBytes {
                    prefix: Prefix::new(prefix_bytes),
                    rex: rex
                })
        )
        .unwrap();

        alt!(
            input,
            apply!(Add::try_parse, prefix)
                | apply!(And::try_parse, prefix)
                | apply!(Call::try_parse, prefix)
                | apply!(Clc::try_parse, prefix)
                | apply!(Cmove::try_parse, prefix)
                | apply!(Cmovne::try_parse, prefix)
                | apply!(Cmp::try_parse, prefix)
                | apply!(Dec::try_parse, prefix)
                | apply!(Inc::try_parse, prefix)
                | apply!(Ja::try_parse, prefix)
                | apply!(Jae::try_parse, prefix)
                | apply!(Je::try_parse, prefix)
                | apply!(Jl::try_parse, prefix)
                | apply!(Jne::try_parse, prefix)
                | apply!(Jns::try_parse, prefix)
                | apply!(Jg::try_parse, prefix)
                | apply!(Jge::try_parse, prefix)
                | apply!(Jmp::try_parse, prefix)
                | apply!(Lea::try_parse, prefix)
                | apply!(Mov::try_parse, prefix)
                | apply!(Movsx::try_parse, prefix)
                | apply!(Movzx::try_parse, prefix)
                | apply!(Nop::try_parse, prefix)
                | apply!(Or::try_parse, prefix)
                | apply!(Pop::try_parse, prefix)
                | apply!(Push::try_parse, prefix)
                | apply!(Ret::try_parse, prefix)
                | apply!(Sar::try_parse, prefix)
                | apply!(Shr::try_parse, prefix)
                | apply!(Shl::try_parse, prefix)
                | apply!(Sete::try_parse, prefix)
                | apply!(Setne::try_parse, prefix)
                | apply!(Sub::try_parse, prefix)
                | apply!(Test::try_parse, prefix)
                | apply!(Xor::try_parse, prefix)
        )
    }
}

impl fmt::Display for Instruction {
    // FIXME: We should figure out how to allow for Intel and AT&T format.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.opcode)?;

        let width = match self.width {
            Width::Byte => "b",
            Width::Word => "",
            Width::DWord => "l",
            Width::QWord => "q",
        };
        write!(f, "{}\t", width.blue())?;

        if let Some(operand) = &self.op_2 {
            write!(f, "{}, ", operand)?;
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

/// Any and all of the bytes that _might_ come before the instruction
///
#[derive(Clone, Copy, Debug)]
pub(crate) struct PrefixBytes {
    prefix: Prefix,
    rex: Option<REX>,
}

impl PrefixBytes {
    pub(crate) fn new_prefix(bytes: &[u8]) -> Self {
        PrefixBytes {
            prefix: Prefix::new(prefixes(bytes).unwrap().1),
            rex: None,
        }
    }

    pub(crate) fn new_none() -> Self {
        PrefixBytes {
            prefix: Prefix::new((None, None, None, None, &b""[..])),
            rex: None,
        }
    }

    pub(crate) fn new_rex(byte: u8) -> Self {
        PrefixBytes {
            prefix: Prefix::new((None, None, None, None, &b""[..])),
            rex: REX::new(byte),
        }
    }

    pub(crate) fn rex(&self) -> Option<REX> {
        self.rex
    }
}

/// All of the X86 Instructions
///
/// FIXME: Opcode is the wrong name for this.
#[derive(Debug, Deserialize, PartialEq, Serialize)]
#[non_exhaustive]
pub(crate) enum Opcode {
    Add,
    And,
    Call,
    Clc,
    Cmove,
    Cmovne,
    Cmp,
    Dec,
    Inc,
    Ja,
    Jae,
    Je,
    Jg,
    Jge,
    Jl,
    Jne,
    Jns,
    Jmp,
    Lea,
    Mov,
    Movsx,
    Movzx,
    Nop,
    Or,
    Pop,
    Push,
    Ret,
    Sar,
    Shr,
    Shl,
    Sete,
    Setne,
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
            Opcode::Clc => "clc",
            Opcode::Cmove => "cmove",
            Opcode::Cmovne => "cmovne",
            Opcode::Cmp => "cmp",
            Opcode::Dec => "dec",
            Opcode::Inc => "inc",
            Opcode::Ja => "ja",
            Opcode::Jae => "jae",
            Opcode::Je => "je",
            Opcode::Jg => "jg",
            Opcode::Jge => "jge",
            Opcode::Jl => "jl",
            Opcode::Jne => "jne",
            Opcode::Jns => "jns",
            Opcode::Jmp => "jmp",
            Opcode::Lea => "lea",
            Opcode::Mov => "mov",
            Opcode::Movsx => "movsx",
            Opcode::Movzx => "movzx",
            Opcode::Nop => "nop",
            Opcode::Or => "or",
            Opcode::Pop => "pop",
            Opcode::Push => "push",
            Opcode::Ret => "ret",
            Opcode::Sar => "sar",
            Opcode::Shr => "shr",
            Opcode::Shl => "shl",
            Opcode::Sete => "sete",
            Opcode::Setne => "setne",
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
#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub(crate) enum Operand {
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
            _ => {
                write!(f, "fixme")?;
                unimplemented!()
            }
        }
    }
}

/// Immediate Operands
///
#[allow(dead_code)]
#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub(crate) enum Immediate {
    Byte(i8),
    Word(i16),
    DWord(i32),
    QWord(i64),
    UByte(u8),
    UWord(u16),
    UDWord(u32),
    UQWord(u64),
}

pub(crate) struct ImmediateBuilder {
    width: Width,
}

impl ImmediateBuilder {
    pub(crate) fn new(w: Width) -> Self {
        ImmediateBuilder { width: w }
    }

    pub(crate) fn signed<T: Signed + ToPrimitive>(&self, i: T) -> Operand {
        match self.width {
            Width::Byte => Operand::Immediate(Immediate::Byte(i.to_i8().unwrap())),
            Width::Word => Operand::Immediate(Immediate::Word(i.to_i16().unwrap())),
            Width::DWord => Operand::Immediate(Immediate::DWord(i.to_i32().unwrap())),
            Width::QWord => Operand::Immediate(Immediate::QWord(i.to_i64().unwrap())),
            _ => unreachable!(),
        }
    }

    pub(crate) fn unsigned<T: Unsigned + ToPrimitive>(&self, i: T) -> Operand {
        match self.width {
            Width::Byte => Operand::Immediate(Immediate::UByte(i.to_u8().unwrap())),
            Width::Word => Operand::Immediate(Immediate::UWord(i.to_u16().unwrap())),
            Width::DWord => Operand::Immediate(Immediate::UDWord(i.to_u32().unwrap())),
            Width::QWord => Operand::Immediate(Immediate::UQWord(i.to_u64().unwrap())),
            _ => unreachable!(),
        }
    }
}

impl Immediate {
    pub(crate) fn imm8(i: i8) -> Operand {
        Operand::Immediate(Immediate::Byte(i))
    }

    pub(crate) fn imm32(i: i32) -> Operand {
        Operand::Immediate(Immediate::DWord(i))
    }
}

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // let mut s = String::new();
        // match self {
        //     Immediate::Byte(n) => write!(&mut s, "0x{:x}", n)?,
        //     Immediate::DWord(n) => write!(&mut s, "0x{:x}", n)?,
        //     Immediate::UByte(n) => write!(&mut s, "0x{:x}", n)?,
        //     Immediate::UDWord(n) => write!(&mut s, "0x{:x}", n)?,
        // };

        let s = match self {
            Immediate::Byte(n) => n.to_string(),
            Immediate::Word(n) => n.to_string(),
            Immediate::DWord(n) => n.to_string(),
            Immediate::QWord(n) => n.to_string(),
            Immediate::UByte(n) => n.to_string(),
            Immediate::UWord(n) => n.to_string(),
            Immediate::UDWord(n) => n.to_string(),
            Immediate::UQWord(n) => n.to_string(),
        };
        write!(f, "{}", s.cyan())
    }
}

#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct LogicalAddress {
    pub(crate) segment: Option<Register>,
    pub(crate) offset: EffectiveAddress,
}

impl fmt::Display for LogicalAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.offset)
    }
}

#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct EffectiveAddress {
    pub(crate) base: Option<Register>,
    pub(crate) index: Option<Register>,
    pub(crate) scale: Option<ScaleValue>,
    pub(crate) displacement: Option<Displacement>,
}

impl fmt::Display for EffectiveAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(displacement) = self.displacement {
            write!(f, "{}", displacement)?;
        }
        match (&self.base, &self.index, &self.scale) {
            (Some(b), Some(i), Some(s)) => write!(f, "(%{},%{},{})", b, i, s),
            (Some(b), Some(i), None) => write!(f, "(%{},%{})", b, i),
            (Some(b), None, None) => write!(f, "(%{})", b),
            (None, None, None) => Ok(()),
            _ => {
                println!("{:?}", self);
                unimplemented!()
            }
        }
    }
}

#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub(crate) enum ScaleValue {
    Two = 2,
    Four = 4,
    Eight = 8,
}

impl fmt::Display for ScaleValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScaleValue::Two => write!(f, "2"),
            ScaleValue::Four => write!(f, "4"),
            ScaleValue::Eight => write!(f, "8"),
        }
    }
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) enum Displacement {
    Byte(i8),
    Word(i16),
    DWord(i32),
}

impl fmt::Display for Displacement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        match self {
            // Displacement::Byte(n) => write!(&mut s, "0x{:x}", n)?,
            // Displacement::Word(n) => write!(&mut s, "0x{:x}", n)?,
            // Displacement::DWord(n) => write!(&mut s, "0x{:x}", n)?,
            Displacement::Byte(n) => write!(&mut s, "{}", n)?,
            Displacement::Word(n) => write!(&mut s, "{}", n)?,
            Displacement::DWord(n) => write!(&mut s, "{}", n)?,
        };
        write!(f, "{}", s.red())
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
                    width: Width::QWord,
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
                    width: Width::QWord,
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
                    width: Width::QWord,
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
                    width: Width::QWord,
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
                    width: Width::QWord,
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
                width: Width::QWord,
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
                width: Width::QWord,
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
                width: Width::QWord,
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
                width: Width::QWord,
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
                width: Width::QWord,
                op_1: None,
                op_2: None,
                op_3: None,
            },
            "ret"
        );

        assert_eq!(remainder, &b""[..]);
    }
}
