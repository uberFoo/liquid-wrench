use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
pub(crate) struct Je {}

impl DecodeInstruction for Je {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Je::parse_x74, rex) | call!(Je::parse_x0f84, rex)
        )
    }
}

impl Je {
    // 74 cb        => JE rel8
    instr!(parse_x74, Opcode::Je, [0x74], rel8);
    instr!(parse_x0f84, Opcode::Je, [0x0f, 0x84], rel32);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Jne {}

impl DecodeInstruction for Jne {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Jne::parse_x75, rex) | call!(Jne::parse_x0f85, rex)
        )
    }
}

impl Jne {
    // 75 cb        => JNE rel8
    instr!(parse_x75, Opcode::Jne, [0x75], rel8);

    // 0f 85 cw     => JNE rel16
    // 0f 85 cd     => JNE rel32
    instr!(parse_x0f85, Opcode::Jne, [0x0f, 0x85], rel32);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Jg {}

impl DecodeInstruction for Jg {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        call!(input, Jg::parse_x7f, rex)
    }
}

impl Jg {
    // 7f cb        => JG rel8
    instr!(parse_x7f, Opcode::Jg, [0x7f], rel8);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Jge {}

impl DecodeInstruction for Jge {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        call!(input, Jge::parse_x7d, rex)
    }
}

impl Jge {
    // 7d cb        => JGE rel8
    instr!(parse_x7d, Opcode::Jge, [0x7d], rel8);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Ja {}

impl DecodeInstruction for Ja {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        call!(input, Ja::parse_x0f87, rex)
    }
}

impl Ja {
    // 0f 87 cw        => JA rel16
    // 0f 87 cd        => JA rel32
    instr!(parse_x0f87, Opcode::Ja, [0x0f, 0x87], rel32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::instr::{
        Displacement, EffectiveAddress, LogicalAddress, Operand::Memory as OpMem,
    };

    #[test]
    fn instr_je() {
        assert_eq!(
            Je::try_parse(b"\x74\x5e", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Je,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(94_i8))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "74 5e   je      94"
        );
    }

    #[test]
    fn instr_jne() {
        assert_eq!(
            Jne::try_parse(b"\x75\x07", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jne,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(7_i8))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "75 07   jne     7"
        );
    }

    #[test]
    fn instr_jg() {
        assert_eq!(
            Jg::try_parse(b"\x7f\x1a", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jg,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(26_i8))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "7f 1a   jg      26"
        );

        assert_eq!(
            Jg::try_parse(b"\x7f\xff", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jg,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(-1_i8))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "7f ff   jg      -1"
        );
    }

    #[test]
    fn instr_jge() {
        assert_eq!(
            Jge::try_parse(b"\x7d\x07", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jge,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(7_i8))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "7d 07   jg      7"
        );
    }

    #[test]
    fn instr_ja() {
        assert_eq!(
            Ja::try_parse(b"\x0f\x87\x81\x03\x00\x00", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Ja,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::DWord(897))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "0f 87 81 03 00 00       ja      897"
        );
    }
}
