use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
crate struct Test {}

impl DecodeInstruction for Test {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Test::parse_x85, rex))
    }
}

impl Test {
    // 85 /r            => TEST r/m16, r16
    // 85 /r            => TEST r/m32, r32
    // REX.W + 85 /r    => TEST r/m64, r64
    instr!(parse_x85, Opcode::Test, [0x85], r/m32, /r32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{instr::Operand::Register as OpReg, register::ctors::*};

    #[test]
    fn instr_test_85() {
        assert_eq!(
            Test::try_parse(b"\x85\xff", None),
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
