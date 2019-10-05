use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Call {}

impl DecodeInstruction for Call {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Call::parse_xe8, prefix, address) | call!(Call::parse_xff, prefix, address)
        )
    }
}

impl Call {
    // E8 cw            => CALL rel16
    // E8 cd            => CALL rel32
    instr!(parse_xe8, Opcode::Call, Width::QWord, [0xe8], rel32);

    // ff /2            => CALL r/m16
    // ff /2            => CALL r/m32
    // ff /2            => CALL r/m64
    instr!(parse_xff, Opcode::Call, Width::QWord, [0xff]+/2, r/m32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{Displacement, EffectiveAddress, LogicalAddress, Operand::Memory as OpMem},
        register::ctors::*,
    };

    #[test]
    fn instr_call_e8() {
        assert_eq!(
            Call::try_parse(&[0xe8, 0x38, 0x32, 0x00, 0x00], PrefixBytes::new_none(), 0),
            Ok((
                &[][..],
                Instruction {
                    address: 0,
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

        #[test]
        fn instr_call_ff() {
            assert_eq!(
                Call::try_parse(
                    &[0xff, 0x15, 0xbf, 0x3c, 0x00, 0x00],
                    PrefixBytes::new_none(),
                    0
                ),
                Ok((
                    &[][..],
                    Instruction {
                        address: 0,
                        opcode: Opcode::Call,
                        width: Width::QWord,
                        op_1: Some(OpMem(LogicalAddress {
                            segment: None,
                            offset: EffectiveAddress {
                                base: Some(rip()),
                                index: None,
                                scale: None,
                                displacement: Some(Displacement::DWord(15551))
                            }
                        })),
                        op_2: None,
                        op_3: None,
                    }
                )),
                "ff 15 bf 3c 00 00       callq   *15551(%rip)"
            );
        }

        // ff d1 	callq	*%rcx
        // ff 9c ff ff ff 1e fe 	lcalll	*-31522817(%rdi,%rdi,8)
        // ff 55 48 	callq	*72(%rbp)
    }
}
