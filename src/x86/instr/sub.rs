use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
crate struct Sub {}

impl DecodeInstruction for Sub {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Sub::parse_x81, rex))
    }
}

impl Sub {
    // 81 /5 id         => SUB r/m16, imm16
    // 81 /5 iw         => SUB r/m32, imm32
    // REX.W + 81 /5 id => SUB r/m64, imm32
    instr!(parse_x81, Opcode::Sub, [0x81]+/5, r/m32, imm32);
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
            Sub::try_parse(b"\x81\xec\x18\x06\x00\x00", REX::new(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Sub,
                    op_1: Some(OpReg(rsp())),
                    op_2: Some(OpImm(Immediate::DWord(1560_i32))),
                    op_3: None
                }
            )),
            "48 81 ec 18 06 00 00    subq    $1560, %rsp"
        );
    }
}
