use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes};

#[derive(Debug, PartialEq)]
pub(crate) struct Test {}

impl DecodeInstruction for Test {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Test::parse_x84, prefix)
                | call!(Test::parse_x85, prefix)
                | call!(Test::parse_xf6, prefix)
        )
    }
}

impl Test {
    // 84 /r            => TEST r/m8, r8
    // REX + 84 /r      => TEST r/m8, r8
    instr!(parse_x84, Opcode::Test, [0x84], r/m8, /r8);

    // 85 /r            => TEST r/m16, r16
    // 85 /r            => TEST r/m32, r32
    // REX.W + 85 /r    => TEST r/m64, r64
    instr!(parse_x85, Opcode::Test, [0x85], r/m32, /r32);

    // F6 /0 ib         => TEST r/m8, imm8
    // REX F6 /0 ib     => TEST r/m8, imm8
    instr!(parse_xf6, Opcode::Test, [0xf6]+/0, r/m8, imm8);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{instr::Operand::Register as OpReg, register::ctors::*};

    #[test]
    fn instr_test_84() {
        assert_eq!(
            Test::try_parse(b"\x84\xc0", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Test,
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
                    op_1: Some(OpReg(edi())),
                    op_2: Some(OpReg(edi())),
                    op_3: None
                }
            )),
            "85 ff   testl   %edi, %ed"
        );
    }
}
