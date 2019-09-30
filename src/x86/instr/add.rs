use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes};

#[derive(Debug, PartialEq)]
pub(crate) struct Add {}

impl DecodeInstruction for Add {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Add::parse_x01, prefix) | call!(Add::parse_x83, prefix)
        )
    }
}

impl Add {
    // 01 /r            => ADD r/m16, r16
    // 01 /r            => ADD r/m32, r32
    // REX.W + 01 /r    => ADD r/m64, r64
    instr!(parse_x01, Opcode::Add, [0x01], r/m32, /r32);

    // 83 /0 ib         => ADD r/m16, imm8
    // 83 /0 ib         => ADD r/m32, imm8
    // REX.W + 83 /0 ib => ADD r/m64, imm8
    instr!(parse_x83, Opcode::Add, [0x83]+/0, r/m32, imm8);
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
    fn instr_add_01() {
        assert_eq!(
            Add::try_parse(b"\x01\xd8", PrefixBytes::new_rex(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Add,
                    op_1: Some(OpReg(rax())),
                    op_2: Some(OpReg(rbx())),
                    op_3: None
                }
            )),
            "48 01 d8        addq    %rbx, %rax"
        );
    }

    #[test]
    fn instr_add_83() {
        assert_eq!(
            Add::try_parse(b"\x83\xc7\x68", PrefixBytes::new_rex(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Add,
                    op_1: Some(OpReg(rdi())),
                    op_2: Some(OpImm(Immediate::Byte(104))),
                    op_3: None
                }
            )),
            "48 83 c7 68     addq    $104, %rdi"
        );
    }
}
