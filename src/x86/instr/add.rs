use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
crate struct Add {}

impl DecodeInstruction for Add {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Add::parse_x83, rex))
    }
}

impl Add {
    // 83 /0 ib         => AND r/m16, imm8
    // 83 /0 ib         => AND r/m32, imm8
    // REX.W + 83 /0 ib => AND r/m64, imm8
    instr!(parse_x83, Opcode::And, [0x83]+/0, r/m32, imm8);
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
    fn instr_add_83() {
        assert_eq!(
            Add::try_parse(b"\x83\xc7\x68", REX::new(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::And,
                    op_1: Some(OpReg(rdi())),
                    op_2: Some(OpImm(Immediate::Byte(104))),
                    op_3: None
                }
            )),
            "48 83 c7 68     addq    $104, %rdi"
        );
    }
}
