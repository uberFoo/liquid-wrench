use failure::{err_msg, format_err, Error, Fail};
use nom::*;

crate mod and;
crate mod mov;
crate mod pop;
crate mod push;
crate mod ret;
crate mod xor;

use self::{and::And, mov::Mov, pop::Pop, push::Push, ret::Ret, xor::Xor};
use crate::x86::modrm::REX;
use crate::x86::register::*;

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
    crate segment: Option<Register>,
    crate offset: EffectiveAddress,
}

#[derive(Debug, PartialEq)]
crate struct EffectiveAddress {
    crate base: Register,
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

#[derive(Copy, Clone, Debug, PartialEq)]
crate enum Displacement {
    Byte(i8),
    DWord(i32),
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
