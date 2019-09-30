use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
pub(crate) struct Jmp {}

impl DecodeInstruction for Jmp {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Jmp::parse_xe9, rex) | call!(Jmp::parse_xeb, rex) | call!(Jmp::parse_xff4, rex)
        )
    }
}

impl Jmp {
    // e9 cw            => JMP rel16
    // e9 cd            => JMP rel32
    instr!(parse_xe9, Opcode::Jmp, [0xe9], rel32);

    // eb cb            => JMP rel8
    instr!(parse_xeb, Opcode::Jmp, [0xeb], rel8);

    // ff /4            => JMP r/m64
    instr!(parse_xff4, Opcode::Jmp, [0xff]+/4, r/m64);
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
            Jmp::try_parse(b"\xe9\x62\x36\x00\x00", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jmp,
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
            Jmp::try_parse(b"\xeb\x11", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jmp,
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
