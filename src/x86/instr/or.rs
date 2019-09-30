use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
pub(crate) struct Or {}

impl DecodeInstruction for Or {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Or::parse_x09, rex) | call!(Or::parse_x81, rex) | call!(Or::parse_x83, rex)
        )
    }
}

impl Or {
    // 09 /r            => OR r/m16, r16
    // 09 /r            => OR r/m32, r32
    // REX.W + 09 /r    => OR r/m64, r64
    instr!(parse_x09, Opcode::Or, [0x09], r/m32, /r32);

    // 81 /1 /iw            => OR r/m16, imm16
    // 81 /1 /id            => OR r/m32, imm32
    // REX.W + 81 /1 id     => OR r/m64, imm32
    instr!(parse_x81, Opcode::Or, [0x81]+/1, r/m32, imm32);

    // 83 /1 ib         => OR r/m16, imm8
    // 83 /1 ib         => OR r/m32, imm8
    // REX.W + 83 /1 ib => OR r/m64, imm8
    instr!(parse_x83, Opcode::Or, [0x83]+/1, r/m32, imm8);
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
    fn instr_or_83() {
        assert_eq!(
            Or::try_parse(b"\x83\xcc\x01", REX::new(0x41)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Or,
                    op_1: Some(OpReg(r12d())),
                    op_2: Some(OpImm(Immediate::Byte(1))),
                    op_3: None
                }
            )),
            "41 83 cc 01     orl     $1, %r12d"
        );
    }
}
