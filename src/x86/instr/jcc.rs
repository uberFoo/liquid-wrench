use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

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
    instr!(parse_x77, Opcode::Ja, Width::Word, [0x77], rel8);
    // 0f 87 cw        => JA rel16
    // 0f 87 cd        => JA rel32
    instr!(parse_x0f87, Opcode::Ja, Width::Word, [0x0f, 0x87], rel32);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Jae {}

impl DecodeInstruction for Jae {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Jae::parse_x73, prefix))
    }
}

impl Jae {
    // 73 cb        => JAE rel8
    instr!(parse_x73, Opcode::Jae, Width::Word, [0x73], rel8);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Jb {}

impl DecodeInstruction for Jb {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Jb::parse_x72, prefix))
    }
}

impl Jb {
    // 72 cb        => JB rel8
    instr!(parse_x72, Opcode::Jb, Width::Word, [0x72], rel8);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Jbe {}

impl DecodeInstruction for Jbe {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Jbe::parse_x76, prefix))
    }
}

impl Jbe {
    // 76 cb        => JBE rel8
    instr!(parse_x76, Opcode::Jbe, Width::Word, [0x76], rel8);
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
    instr!(parse_x74, Opcode::Je, Width::Word, [0x74], rel8);
    // 0f 84 cd     => JE rel32
    instr!(parse_x0f84, Opcode::Je, Width::Word, [0x0f, 0x84], rel32);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Jl {}

impl DecodeInstruction for Jl {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Jl::parse_x7c, prefix))
    }
}

impl Jl {
    // 7c cb        => JL rel8
    instr!(parse_x7c, Opcode::Jl, Width::Word, [0x7c], rel8);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Jle {}

impl DecodeInstruction for Jle {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Jle::parse_x7e, prefix) | call!(Jle::parse_x0f8e, prefix)
        )
    }
}

impl Jle {
    // 7e cb        => JLE rel8
    instr!(parse_x7e, Opcode::Jle, Width::Word, [0x7e], rel8);
    // 0f 8e cw     => JLE rel16
    // 0f 8e cd     => JLE rel32
    instr!(parse_x0f8e, Opcode::Jle, Width::Word, [0x0f, 0x8e], rel32);
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
    instr!(parse_x75, Opcode::Jne, Width::Word, [0x75], rel8);

    // 0f 85 cw     => JNE rel16
    // 0f 85 cd     => JNE rel32
    instr!(parse_x0f85, Opcode::Jne, Width::Word, [0x0f, 0x85], rel32);
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
    instr!(parse_x79, Opcode::Jns, Width::Word, [0x79], rel8);
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
    instr!(parse_x7f, Opcode::Jg, Width::Word, [0x7f], rel8);
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
    instr!(parse_x7d, Opcode::Jge, Width::Word, [0x7d], rel8);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::instr::{
        Displacement, EffectiveAddress, LogicalAddress, Operand::Memory as OpMem,
    };

    #[test]
    fn instr_ja_0f87() {
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
    }

    #[test]
    fn instr_ja_77() {
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

    #[test]
    fn instr_jae_73() {
        assert_eq!(
            Jae::try_parse(b"\x73\x07", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jae,
                    width: Width::Word,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(7))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "73 07   jae     7"
        );
    }

    #[test]
    fn instr_jb_72() {
        assert_eq!(
            Jb::try_parse(b"\x72\x37", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jb,
                    width: Width::Word,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(55))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "72 37   jb      55"
        );
    }

    #[test]
    fn instr_jbe_76() {
        assert_eq!(
            Jbe::try_parse(b"\x76\x07", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jbe,
                    width: Width::Word,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(7))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "76 07   jbe     7"
        );
    }

    #[test]
    fn instr_je_74() {
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
    fn instr_jl_7c() {
        assert_eq!(
            Jl::try_parse(b"\x7c\x19", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jl,
                    width: Width::Word,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(25_i8))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "7c 19   jl      25"
        );
    }

    #[test]
    fn instr_jle_7e() {
        assert_eq!(
            Jle::try_parse(b"\x7e\x23", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Jle,
                    width: Width::Word,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: None,
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(35_i8))
                        }
                    })),
                    op_2: None,
                    op_3: None
                }
            )),
            "7e 23   jle      35"
        );
    }

    #[test]
    fn instr_jne_75() {
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
    fn instr_jg_7f() {
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
    fn instr_jge_7d() {
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
}
