use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Jmp {}

impl DecodeInstruction for Jmp {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Jmp::parse_xe9, prefix)
                | call!(Jmp::parse_xeb, prefix)
                | call!(Jmp::parse_xff4, prefix)
        )
    }
}

impl Jmp {
    // e9 cw            => JMP rel16
    // e9 cd            => JMP rel32
    instr!(parse_xe9, Opcode::Jmp, Width::Word, [0xe9], rel32);

    // eb cb            => JMP rel8
    instr!(parse_xeb, Opcode::Jmp, Width::Word, [0xeb], rel8);

    // ff /4            => JMP r/m64
    instr!(parse_xff4, Opcode::Jmp, Width::Word, [0xff]+/4, r/m64);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::instr::{
        Displacement, EffectiveAddress, LogicalAddress, Operand::Memory as OpMem,
    };

    #[test]
    fn instr_jmp_e9() {
        assert_eq!(
            Jmp::try_parse(b"\xe9\x62\x36\x00\x00", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jmp,
                    width: Width::Word,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::DWord(13922_i32))
                        }
                    })),
                    op_2: None,
                    op_3: None,
                }
            )),
            "jmp     13922"
        );
    }

    #[test]
    fn instr_jmp_eb() {
        assert_eq!(
            Jmp::try_parse(b"\xeb\x11", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jmp,
                    width: Width::Word,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(17_i8))
                        }
                    })),
                    op_2: None,
                    op_3: None,
                }
            )),
            "eb 11   jmp     17"
        );
    }
}
