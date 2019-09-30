use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
pub(crate) struct Nop {}

impl DecodeInstruction for Nop {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Nop::parse_x90, rex))
    }
}

impl Nop {
    // 90       => NOP
    instr!(parse_x90, Opcode::Nop, [0x90]);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instr_nop_90() {
        assert_eq!(
            Nop::try_parse(b"\x90", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Nop,
                    op_1: None,
                    op_2: None,
                    op_3: None
                }
            ))
        );
    }

    #[test]
    fn two_byte_nop() {
        assert_eq!(
            Nop::try_parse(b"\x66\x90", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Nop,
                    op_1: None,
                    op_2: None,
                    op_3: None
                }
            ))
        );
    }
}
