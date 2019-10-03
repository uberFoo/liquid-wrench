use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Add {}

impl DecodeInstruction for Add {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Add::parse_x01, prefix)
                | call!(Add::parse_x81, prefix)
                | call!(Add::parse_x83, prefix)
        )
    }
}

impl Add {
    // 01 /r            => ADD r/m16, r16
    // 01 /r            => ADD r/m32, r32
    // REX.W + 01 /r    => ADD r/m64, r64
    instr!(parse_x01, Opcode::Add, Width::DWord, [0x01], r/m32, /r32);

    // 81 /0 iw             => ADD r/m16, imm16
    // 81 /0 id             => ADD r/m32, imm32
    // REX.W + 81 /0 id     => ADD r/m64, imm32
    instr!(parse_x81, Opcode::Add, Width::DWord, [0x81]+/0, r/m32, imm32);

    // 83 /0 ib         => ADD r/m16, imm8
    // 83 /0 ib         => ADD r/m32, imm8
    // REX.W + 83 /0 ib => ADD r/m64, imm8
    instr!(parse_x83, Opcode::Add, Width::DWord, [0x83]+/0, r/m32, imm8);
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
        Width,
    };

    #[test]
    fn instr_add_01() {
        assert_eq!(
            Add::try_parse(b"\x01\xd8", PrefixBytes::new_rex(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Add,
                    width: Width::QWord,
                    op_1: Some(OpReg(rax())),
                    op_2: Some(OpReg(rbx())),
                    op_3: None
                }
            )),
            "48 01 d8        addq    %rbx, %rax"
        );
    }

    #[test]
    fn instr_add_81() {
        assert_eq!(
            Add::try_parse(b"\x81\xc4\xa8\x00\x00\x00", PrefixBytes::new_rex(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Add,
                    width: Width::QWord,
                    op_1: Some(OpReg(rsp())),
                    op_2: Some(OpImm(Immediate::DWord(168))),
                    op_3: None
                }
            )),
            "48 81 c4 a8 00 00 00    addq    $168, %rsp"
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
                    width: Width::QWord,
                    op_1: Some(OpReg(rdi())),
                    op_2: Some(OpImm(Immediate::Byte(104))),
                    op_3: None
                }
            )),
            "48 83 c7 68     addq    $104, %rdi"
        );
    }
}
