use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes};

#[derive(Debug, PartialEq)]
pub(crate) struct Clc {}

impl DecodeInstruction for Clc {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Clc::parse_xf8, prefix))
    }
}

impl Clc {
    // f8       => CLC
    instr!(parse_xf8, Opcode::Clc, [0xf8]);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::Width;

    #[test]
    fn instr_clc_f8() {
        assert_eq!(
            Clc::try_parse(b"\xf8", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
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
