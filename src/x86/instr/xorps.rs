use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Xorps {}

impl DecodeInstruction for Xorps {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Xorps::parse_x0f57, prefix))
    }
}

impl Xorps {
    // 0f 57 /r     => XORPS xmm1, xmm2/m128
    instr!(parse_x0f57, Opcode::Xorps, Width::Word, [0x0f, 0x57], /xmm, /xmm);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{
            Immediate,
            Operand::{Immediate as OpImm, Register as OpReg},
        },
        register::ctors::*,
    };

    #[test]
    fn instr_xorps_0f57() {
        assert_eq!(
            Xorps::try_parse(b"\x0f\x57\xc0", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Xorps,
                    width: Width::Word,
                    op_1: Some(OpReg(xmm0())),
                    op_2: Some(OpReg(xmm0())),
                    op_3: None
                }
            )),
            "0f 57 c0        xorps   %xmm0, %xmm0"
        );
    }
}
