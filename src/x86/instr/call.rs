use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Call {}

impl DecodeInstruction for Call {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Call::parse_xe8, prefix))
    }
}

impl Call {
    // E8 cw            => CALL rel16
    // E8 cd            => CALL rel32
    instr!(parse_xe8, Opcode::Call, Width::QWord, [0xe8], rel32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::instr::{
        Displacement, EffectiveAddress, LogicalAddress, Operand::Memory as OpMem,
    };

    #[test]
    fn instr_call_e8() {
        assert_eq!(
            Call::try_parse(&[0xe8, 0x38, 0x32, 0x00, 0x00], PrefixBytes::new_none()),
            Ok((
                &[][..],
                Instruction {
                    opcode: Opcode::Call,
                    width: Width::QWord,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::DWord(12856_i32))
                        }
                    })),
                    op_2: None,
                    op_3: None,
                }
            )),
            "e8 38 32 00 00 	callq	12856"
        );

        // ff d1 	callq	*%rcx
        // ff 9c ff ff ff 1e fe 	lcalll	*-31522817(%rdi,%rdi,8)
        // ff 55 48 	callq	*72(%rbp)
    }
}
