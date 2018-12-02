use nom::*;

use crate::x86::{
    instr::{
        DecodeInstruction, Immediate, Instruction, Opcode,
        Operand::{Immediate as OpImm, Register as OpReg},
        REX,
    },
    modrm::ModRM,
    register::ctors::eax,
};

#[derive(Debug, PartialEq)]
crate struct And {}

impl DecodeInstruction for And {
    #[allow(clippy::cyclomatic_complexity)]
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(And::parse_20, rex)
                | call!(And::parse_21, rex)
                | call!(And::parse_22, rex)
                | call!(And::parse_23, rex)
                | call!(And::parse_24, rex)
                | call!(And::parse_25, rex)
                | call!(And::parse_80, rex)
                | call!(And::parse_81, rex)
                | call!(And::parse_83, rex)
        )
    }
}

impl And {
    named_args!(
        parse_20(rex: Option<REX>)<Instruction>,
        do_parse!(
            tag!(b"\x20")
            >> modrm: call!(ModRM::new, rex)
            >> (Instruction {
                opcode: Opcode::And,
                op_1: Some(modrm.r_m8()),
                op_2: Some(modrm.r8()),
                op_3: None
            })
        )
    );

    named_args!(
        parse_21(rex: Option<REX>)<Instruction>,
        do_parse!(
            tag!(b"\x21")
            >> modrm: call!(ModRM::new, rex)
            >> (Instruction {
                opcode: Opcode::And,
                op_1: Some(modrm.r_m32()),
                op_2: Some(modrm.r32()),
                op_3: None
            })
        )
    );

    named_args!(
        parse_22(rex: Option<REX>)<Instruction>,
        do_parse!(
            tag!(b"\x22")
            >> (Instruction {
                opcode: Opcode::And,
                op_1: None,
                op_2: None,
                op_3: None
            })
        )
    );

    named_args!(
        parse_23(rex: Option<REX>)<Instruction>,
        do_parse!(
            tag!(b"\x23")
            >> (Instruction {
                opcode: Opcode::And,
                op_1: None,
                op_2: None,
                op_3: None
            })
        )
    );

    named_args!(
        parse_24(rex: Option<REX>)<Instruction>,
        do_parse!(
            tag!(b"\x24")
            >> (Instruction {
                opcode: Opcode::And,
                op_1: Some(OpReg(eax())),
                op_2: None,
                op_3: None
            })
        )
    );

    named_args!(
        parse_25(rex: Option<REX>)<Instruction>,
        do_parse!(
            tag!(b"\x25")
            >> imm: le_i32
            >> (Instruction {
                opcode: Opcode::And,
                op_1: Some(OpReg(eax())),
                op_2: Some(OpImm(Immediate::DWord(imm))),
                op_3: None
            })
        )
    );

    named_args!(
        parse_80(rex: Option<REX>)<Instruction>,
        do_parse!(
            tag!(b"\x80")
            // Here we are testing the opcode extension bits of the ModR/M byte.  If they are 0b100
            // then "this" 0x83 is an AND instruction. Otherwise, it's something else, and we fail
            // to give another parser a try.
            >> peek!(bits!(do_parse!(
                take_bits!(u8, 2) >> tag_bits!(u8, 3, 0b100) >> ()
            )))
            >> modrm: call!(ModRM::new, rex)
            >> imm: le_i8
            >> (Instruction {
                opcode: Opcode::And,
                op_1: Some(modrm.r_m8()),
                op_2: Some(OpImm(Immediate::Byte(imm))),
                op_3: None
            })
        )
    );

    named_args!(
        parse_81(rex: Option<REX>)<Instruction>,
        do_parse!(
            tag!(b"\x81")
            // Here we are testing the opcode extension bits of the ModR/M byte.  If they are 0b100
            // then "this" 0x83 is an AND instruction. Otherwise, it's something else, and we fail
            // to give another parser a try.
            >> peek!(bits!(do_parse!(
                take_bits!(u8, 2) >> tag_bits!(u8, 3, 0b100) >> ()
            )))
            >> modrm: call!(ModRM::new, rex)
            >> imm: le_u32
            >> (Instruction {
                opcode: Opcode::And,
                op_1: Some(modrm.r_m32()),
                op_2: Some(OpImm(Immediate::UDWord(imm))),
                op_3: None
            })
        )
    );

    named_args!(
        parse_83(rex: Option<REX>)<Instruction>,
        do_parse!(
            tag!(b"\x83")
            // Here we are testing the opcode extension bits of the ModR/M byte.  If they are 0b100
            // then "this" 0x83 is an AND instruction. Otherwise, it's something else, and we fail
            // to give another parser a try.
                >> peek!(bits!(do_parse!(
                    take_bits!(u8, 2) >> tag_bits!(u8, 3, 0b100) >> ()
                )))
                >> modrm: call!(ModRM::new, rex)
                >> imm: le_i8
                >> (Instruction {
                    opcode: Opcode::And,
                    op_1: Some(modrm.r_m32()),
                    op_2: Some(OpImm(Immediate::Byte(imm))),
                    op_3: None
                })
        )
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{instr::Operand::Register as OpReg, register::ctors::*};

    #[test]
    fn instr_and_20() {
        assert_eq!(
            And::try_parse(b"\x20\xcb", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(bl())),
                    op_2: Some(OpReg(cl())),
                    op_3: None
                }
            )),
            "20 cb 	andb	%cl, %bl"
        );

        assert_eq!(
            And::try_parse(b"\x20\xf1", REX::new(0x40)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(cl())),
                    op_2: Some(OpReg(sil())),
                    op_3: None
                }
            )),
            "40 20 f1 	andb	%sil, %cl"
        );

        assert_eq!(
            And::try_parse(b"\x20\xf0", REX::new(0x44)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(al())),
                    op_2: Some(OpReg(r14l())),
                    op_3: None
                }
            )),
            "44 20 f0 	andb	%r14b, %al"
        );
    }

    #[test]
    fn instr_and_21() {
        assert_eq!(
            And::try_parse(b"\x21\xc8", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(eax())),
                    op_2: Some(OpReg(ecx())),
                    op_3: None
                }
            )),
            "21 c8 	andl	%ecx, %eax"
        );

        assert_eq!(
            And::try_parse(b"\x21\xc1", REX::new(40)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(rcx())),
                    op_2: Some(OpReg(rax())),
                    op_3: None
                }
            )),
            "48 21 c1 	andq	%rax, %rcx"
        );
    }

    // '24 fe 	andb	$-2, %al"

    #[test]
    fn instr_and_25() {
        assert_eq!(
            And::try_parse(b"\x25\x00\xf0\x00\x00", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(eax())),
                    op_2: Some(OpImm(Immediate::DWord(61_440_i32))),
                    op_3: None
                }
            )),
            "25 00 f0 00 00 	andl	$61440, %eax"
        )
    }

    #[test]
    fn instr_and_80() {
        assert_eq!(
            And::try_parse(b"\x80\xe1\xf8", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(cl())),
                    op_2: Some(OpImm(Immediate::Byte(-8_i8))),
                    op_3: None
                }
            )),
            "80 e1 f8 	andb	$-8, %cl"
        )
    }

    #[test]
    fn instr_and_81() {
        assert_eq!(
            And::try_parse(b"\x81\xe1\xee\xfb\xff\xff", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(ecx())),
                    // FIXME: Why is this unsigned?
                    op_2: Some(OpImm(Immediate::UDWord(4_294_966_254_u32))),
                    op_3: None
                }
            )),
            "81 e1 ee fb ff ff 	andl	$4294966254, %ecx"
        );

        assert_eq!(
            And::try_parse(b"\x81\xe6\x00\xf0\x00\x00", REX::new(0x41)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(r14d())),
                    op_2: Some(OpImm(Immediate::UDWord(61_440_u32))),
                    op_3: None
                }
            )),
            "41 81 e6 00 f0 00 00 	andl	$61440, %r14d"
        )
    }

    #[test]
    fn instr_and_83() {
        assert_eq!(
            And::try_parse(b"\x83\xe1\x0f", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(ecx())),
                    op_2: Some(OpImm(Immediate::Byte(15_i8))),
                    op_3: None
                }
            )),
            "83 e1 0f 	andl	$15, %ecx"
        );

        assert_eq!(
            And::try_parse(b"\x83\xe7\x07", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(edi())),
                    op_2: Some(OpImm(Immediate::Byte(7_i8))),
                    op_3: None
                }
            )),
            "83 e7 07 	andl	$7, %edi"
        );

        // FIXME: Each of these tests has a different REX byte, which the opcode width reflects.
        // However, I'm not yet doing anything with the REX.W bit, and it's still passing the tests!
        assert_eq!(
            And::try_parse(b"\x83\xe4\xed", REX::new(0x41)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(r12d())),
                    op_2: Some(OpImm(Immediate::Byte(-19_i8))),
                    op_3: None
                }
            )),
            "41 83 e4 ed 	andl	$-19, %r12d"
        );

        assert_eq!(
            And::try_parse(b"\x83\xe4\xf0", REX::new(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(rsp())),
                    op_2: Some(OpImm(Immediate::Byte(-16_i8))),
                    op_3: None
                }
            )),
            "48 83 e4 f0 	andq   $-16, %rsp"
        );
    }
}
