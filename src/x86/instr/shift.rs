use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes};

#[derive(Debug, PartialEq)]
pub(crate) struct Sar {}

impl DecodeInstruction for Sar {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Sar::parse_xc1, prefix))
    }
}

impl Sar {
    // c1 /7 ib             => SAR r/m16, imm8
    // c1 /7 ib             => SAR r/m32, imm8
    // REX.W + c1 /7 ib     => SAR r/m64, imm8
    instr!(parse_xc1, Opcode::Sar, [0xc1]+/7, r / m32, imm8);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Shr {}

impl DecodeInstruction for Shr {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Shr::parse_xc1, prefix))
    }
}

impl Shr {
    // c1 /5 ib             => SHR r/m16, imm8
    // c1 /5 ib             => SHR r/m32, imm8
    // REX.W + c1 /5 ib     => SHR r/m64, imm8
    instr!(parse_xc1, Opcode::Shr, [0xc1]+/5, r / m32, imm8);
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
    fn instr_sar_c1() {
        assert_eq!(
            Sar::try_parse(b"\xc1\xf9\x3f", PrefixBytes::new_rex(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Sar,
                    op_1: Some(OpReg(rcx())),
                    op_2: Some(OpImm(Immediate::Byte(63))),
                    op_3: None
                }
            )),
            "48 c1 f9 3f     sarq    $63, %rcx"
        );
    }

    #[test]
    fn instr_shr_c1() {
        assert_eq!(
            Shr::try_parse(b"\xc1\xe9\x37", PrefixBytes::new_rex(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Shr,
                    op_1: Some(OpReg(rcx())),
                    op_2: Some(OpImm(Immediate::Byte(55))),
                    op_3: None
                }
            )),
            "48 c1 e9 37     shrq    $55, %rcx"
        );
    }
}
