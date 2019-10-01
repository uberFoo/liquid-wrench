use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Sub {}

impl DecodeInstruction for Sub {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Sub::parse_x29, prefix)
                | call!(Sub::parse_x81, prefix)
                | call!(Sub::parse_x83, prefix)
        )
    }
}

impl Sub {
    // 29 /r            => SUB r/m16, r16
    // 29 /r            => SUB r/m32, r32
    // REX.W + 29 /r    => SUB r/m64, r64
    instr!(parse_x29, Opcode::Sub, Width::DWord, [0x29], r/m32, /r32);

    // 81 /5 id         => SUB r/m16, imm16
    // 81 /5 iw         => SUB r/m32, imm32
    // REX.W + 81 /5 id => SUB r/m64, imm32
    instr!(parse_x81, Opcode::Sub, Width::DWord, [0x81]+/5, r/m32, imm32);

    // 83 /5 ib             => SUB r/m16, imm8
    // 83 /5 ib             => SUB r/m32, imm8
    // REX.W + 83 /5 ib     => SUB r/m64, imm8
    instr!(parse_x83, Opcode::Sub, Width::DWord, [0x83]+/5, r/m32, imm8);
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
    fn instr_sub_81() {
        assert_eq!(
            Sub::try_parse(b"\x81\xec\x18\x06\x00\x00", PrefixBytes::new_rex(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Sub,
                    width: Width::QWord,
                    op_1: Some(OpReg(rsp())),
                    op_2: Some(OpImm(Immediate::DWord(1560_i32))),
                    op_3: None
                }
            )),
            "48 81 ec 18 06 00 00    subq    $1560, %rsp"
        );
    }

    #[test]
    fn instr_sub_83() {
        assert_eq!(
            Sub::try_parse(b"\x83\xec\x08", PrefixBytes::new_rex(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Sub,
                    width: Width::QWord,
                    op_1: Some(OpReg(rsp())),
                    op_2: Some(OpImm(Immediate::Byte(8))),
                    op_3: None
                }
            )),
            "48 83 ec 08     subq    $8, %rsp"
        );
    }
}
