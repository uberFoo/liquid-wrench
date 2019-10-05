use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Clc {}

impl DecodeInstruction for Clc {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Clc::parse_xf8, prefix, address))
    }
}

impl Clc {
    // f8       => CLC
    instr!(parse_xf8, Opcode::Clc, Width::Word, [0xf8]);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instr_clc_f8() {
        assert_eq!(
            Clc::try_parse(b"\xf8", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Clc,
                    width: Width::Word,
                    op_1: None,
                    op_2: None,
                    op_3: None
                }
            ))
        );
    }
}
