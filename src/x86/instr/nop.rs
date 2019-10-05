use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Nop {}

impl DecodeInstruction for Nop {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Nop::parse_x90, prefix, address))
    }
}

impl Nop {
    // 90       => NOP
    instr!(parse_x90, Opcode::Nop, Width::Word, [0x90]);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instr_nop_90() {
        assert_eq!(
            Nop::try_parse(b"\x90", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Nop,
                    width: Width::Word,
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
            Nop::try_parse(b"\x90", PrefixBytes::new_prefix(b"\x66\x90"), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Nop,
                    width: Width::Word,
                    op_1: None,
                    op_2: None,
                    op_3: None
                }
            ))
        );
    }
}
