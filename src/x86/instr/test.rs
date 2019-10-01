use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Test {}

impl DecodeInstruction for Test {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Test::parse_x84, prefix)
                | call!(Test::parse_x85, prefix)
                | call!(Test::parse_xa8, prefix)
                | call!(Test::parse_xf6, prefix)
        )
    }
}

impl Test {
    // 84 /r            => TEST r/m8, r8
    // REX + 84 /r      => TEST r/m8, r8
    instr!(parse_x84, Opcode::Test, Width::Byte, [0x84], r/m8, /r8);

    // 85 /r            => TEST r/m16, r16
    // 85 /r            => TEST r/m32, r32
    // REX.W + 85 /r    => TEST r/m64, r64
    instr!(parse_x85, Opcode::Test, Width::DWord, [0x85], r/m32, /r32);

    // a8 ib            => TEST al, imm8
    instr!(parse_xa8, Opcode::Test, Width::Byte, [0xa8], reg: al, imm8);

    // F6 /0 ib         => TEST r/m8, imm8
    // REX F6 /0 ib     => TEST r/m8, imm8
    instr!(parse_xf6, Opcode::Test, Width::Byte, [0xf6]+/0, r/m8, imm8);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{
            Immediate,
            Operand::{Immediate as OpImm, Register as OpReg},
        },
        register::ctors::*,
    };

    #[test]
    fn instr_test_84() {
        assert_eq!(
            Test::try_parse(b"\x84\xc0", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Test,
                    width: Width::Byte,
                    op_1: Some(OpReg(al())),
                    op_2: Some(OpReg(al())),
                    op_3: None
                }
            )),
            "84 c0   testb   %al, %al"
        );
    }

    #[test]
    fn instr_test_85() {
        assert_eq!(
            Test::try_parse(b"\x85\xff", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Test,
                    width: Width::DWord,
                    op_1: Some(OpReg(edi())),
                    op_2: Some(OpReg(edi())),
                    op_3: None
                }
            )),
            "85 ff   testl   %edi, %ed"
        );
    }

    #[test]
    fn instr_test_a8() {
        assert_eq!(
            Test::try_parse(b"\xa8\xfe", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Test,
                    width: Width::Byte,
                    op_1: Some(OpReg(al())),
                    op_2: Some(OpImm(Immediate::Byte(-2))),
                    op_3: None
                }
            )),
            "a8 fe   testb   $-2, %al"
        );
    }

    #[test]
    fn instr_test_f6() {
        assert_eq!(
            Test::try_parse(b"\xf6\xc2\x01", PrefixBytes::new_rex(0x41)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Test,
                    width: Width::Byte,
                    op_1: Some(OpReg(r10b())),
                    op_2: Some(OpImm(Immediate::Byte(1))),
                    op_3: None
                }
            )),
            "41 f6 c2 01     testb   $1, %r10b"
        );
    }
}
