use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Movaps {}

impl DecodeInstruction for Movaps {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Movaps::parse_x0f29, prefix))
    }
}

impl Movaps {
    // 0f 29 /r     => MOVAPS xmm2/m128, xmm1
    instr!(parse_x0f29, Opcode::Movaps, Width::Word, [0x0f, 0x29], xmm/m128, /xmm);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{
            Displacement, EffectiveAddress, LogicalAddress,
            Operand::{Memory as OpMem, Register as OpReg},
        },
        register::ctors::*,
    };

    #[test]
    fn instr_movaps_0f29() {
        assert_eq!(
            Movaps::try_parse(b"\x0f\x29\x41\x70", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Movaps,
                    width: Width::Word,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rcx()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(112))
                        }
                    })),

                    op_2: Some(OpReg(xmm0())),
                    op_3: None
                }
            )),
            "0f 29 41 70     movaps  %xmm0, 112(%rcx)"
        );
    }
}
