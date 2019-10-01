use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes};

#[derive(Debug, PartialEq)]
pub(crate) struct Ja {}

impl DecodeInstruction for Ja {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Ja::parse_x77, prefix) | call!(Ja::parse_x0f87, prefix)
        )
    }
}

impl Ja {
    // 77 cb        => JA rel8
    instr!(parse_x77, Opcode::Ja, [0x77], rel8);
    // 0f 87 cw        => JA rel16
    // 0f 87 cd        => JA rel32
    instr!(parse_x0f87, Opcode::Ja, [0x0f, 0x87], rel32);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Je {}

impl DecodeInstruction for Je {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Je::parse_x74, prefix) | call!(Je::parse_x0f84, prefix)
        )
    }
}

impl Je {
    // 74 cb        => JE rel8
    instr!(parse_x74, Opcode::Je, [0x74], rel8);
    // 0f 84 cd     => JE rel32
    instr!(parse_x0f84, Opcode::Je, [0x0f, 0x84], rel32);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Jne {}

impl DecodeInstruction for Jne {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Jne::parse_x75, prefix) | call!(Jne::parse_x0f85, prefix)
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
pub(crate) struct Jns {}

impl DecodeInstruction for Jns {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Jns::parse_x79, prefix))
    }
}

impl Jns {
    // 79 cb        => JNS rel8
    instr!(parse_x79, Opcode::Jns, [0x79], rel8);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Jg {}

impl DecodeInstruction for Jg {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        call!(input, Jg::parse_x7f, prefix)
    }
}

impl Jg {
    // 7f cb        => JG rel8
    instr!(parse_x7f, Opcode::Jg, [0x7f], rel8);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Jge {}

impl DecodeInstruction for Jge {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        call!(input, Jge::parse_x7d, prefix)
    }
}

impl Jge {
    // 7d cb        => JGE rel8
    instr!(parse_x7d, Opcode::Jge, [0x7d], rel8);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{Displacement, EffectiveAddress, LogicalAddress, Operand::Memory as OpMem},
        Width,
    };

    #[test]
    fn instr_je() {
        assert_eq!(
            Je::try_parse(b"\x74\x5e", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Je,
                    width: Width::Word,
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
            Jne::try_parse(b"\x75\x07", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jne,
                    width: Width::Word,
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
    fn instr_jns_79() {
        assert_eq!(
            Jns::try_parse(b"\x79\x05", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jns,
                    width: Width::Word,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(5_i8))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "79 05   jns     5"
        );
    }

    #[test]
    fn instr_jg() {
        assert_eq!(
            Jg::try_parse(b"\x7f\x1a", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jg,
                    width: Width::Word,
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
            Jg::try_parse(b"\x7f\xff", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jg,
                    width: Width::Word,
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
            Jge::try_parse(b"\x7d\x07", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jge,
                    width: Width::Word,
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
            Ja::try_parse(b"\x0f\x87\x81\x03\x00\x00", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Ja,
                    width: Width::Word,
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

        assert_eq!(
            Ja::try_parse(b"\x77\xfb", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Ja,
                    width: Width::Word,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(-5))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "77 fb   ja      -5"
        );
    }
}
