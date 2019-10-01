use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Lea {}

impl DecodeInstruction for Lea {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Lea::parse_x8d, prefix))
    }
}

impl Lea {
    // 8D /r            => LEA r16, m
    // 8D /r            => LEA r32, m
    // REX.W + 8D /r    => LEA r64, m
    instr!(parse_x8d, Opcode::Lea, Width::DWord, [0x8d], /r32, m);
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
    fn instr_lea_8d() {
        assert_eq!(
            Lea::try_parse(&[0x8d, 0x46, 0x68], PrefixBytes::new_rex(0x48)),
            Ok((
                &[][..],
                Instruction {
                    opcode: Opcode::Lea,
                    width: Width::QWord,
                    op_1: Some(OpReg(rax())),
                    op_2: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rsi()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(104_i8))
                        }
                    })),
                    op_3: None,
                }
            )),
            "48 8d 46 68 	leaq	104(%rsi), %rax"
        );

        assert_eq!(
            Lea::try_parse(&[0x8d, 0x55, 0xc8], PrefixBytes::new_rex(0x48)),
            Ok((
                &[][..],
                Instruction {
                    opcode: Opcode::Lea,
                    width: Width::QWord,
                    op_1: Some(OpReg(rdx())),
                    op_2: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rbp()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(-56_i8))
                        }
                    })),
                    op_3: None,
                }
            )),
            "48 8d 55 c8 	leaq	-56(%rbp), %rdx"
        );

        assert_eq!(
            Lea::try_parse(
                &[0x8d, 0x35, 0xc6, 0x38, 0x00, 0x00],
                PrefixBytes::new_rex(0x48)
            ),
            Ok((
                &[][..],
                Instruction {
                    opcode: Opcode::Lea,
                    width: Width::QWord,
                    op_1: Some(OpReg(rsi())),
                    op_2: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rip()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::DWord(14534_i32))
                        }
                    })),
                    op_3: None,
                }
            )),
            "48 8d 35 c6 38 00 00 	leaq	14534(%rip), %rsi"
        );

        assert_eq!(
            Lea::try_parse(
                &[0x8d, 0x05, 0xca, 0x01, 0x00, 0x00],
                PrefixBytes::new_rex(0x4c)
            ),
            Ok((
                &[][..],
                Instruction {
                    opcode: Opcode::Lea,
                    width: Width::QWord,
                    op_1: Some(OpReg(r8())),
                    op_2: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rip()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::DWord(458_i32))
                        }
                    })),
                    op_3: None,
                }
            )),
            "4c 8d 05 ca 01 00 00 	leaq    0x1ca(%rip),%r8"
        );
    }
}
