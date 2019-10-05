use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Sbb {}

impl DecodeInstruction for Sbb {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Sbb::parse_x83, prefix, address))
    }
}

impl Sbb {
    // 83 /3 ib             => SBB r/m16, imm8
    // 83 /3 ib             => SBB r/m32, imm8
    // REX.W + 83 /3 ib     => SBB r/m32, imm8
    instr!(parse_x83, Opcode::Sbb, Width::DWord, [0x83]+/3, r/m32, imm8);
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
    fn instr_sb_83() {
        assert_eq!(
            Sbb::try_parse(b"\x83\xd8\xff", PrefixBytes::new_rex(0x48), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Sbb,
                    width: Width::QWord,
                    op_1: Some(OpReg(rax())),
                    op_2: Some(OpImm(Immediate::Byte(-1))),
                    op_3: None
                }
            )),
            "48 83 d8 ff     sbbq    $-1, %rax"
        );
    }
}
