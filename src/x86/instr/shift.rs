use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Sar {}

impl DecodeInstruction for Sar {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Sar::parse_xc1, prefix, address))
    }
}

impl Sar {
    // c1 /7 ib             => SAR r/m16, imm8
    // c1 /7 ib             => SAR r/m32, imm8
    // REX.W + c1 /7 ib     => SAR r/m64, imm8
    instr!(parse_xc1, Opcode::Sar, Width::DWord, [0xc1]+/7, r / m32, imm8);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Shl {}

impl DecodeInstruction for Shl {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Shl::parse_xc0, prefix, address) | call!(Shl::parse_xc1, prefix, address)
        )
    }
}

impl Shl {
    // c0 /4 ib             => SHL r/m8, imm8
    // REX + c0 /4 ib     => SHL r/m8, imm8
    instr!(parse_xc0, Opcode::Shl, Width::Byte, [0xc0]+/4, r/m8,  imm8);

    // c1 /4 ib             => SHL r/m16, imm8
    // c1 /4 ib             => SHL r/m32, imm8
    // REX.W + c1 /4 ib     => SHL r/m64, imm8
    instr!(parse_xc1, Opcode::Shl, Width::DWord, [0xc1]+/4, r/m32, imm8);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Shr {}

impl DecodeInstruction for Shr {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Shr::parse_xc1, prefix, address))
    }
}

impl Shr {
    // c1 /5 ib             => SHR r/m16, imm8
    // c1 /5 ib             => SHR r/m32, imm8
    // REX.W + c1 /5 ib     => SHR r/m64, imm8
    instr!(parse_xc1, Opcode::Shr, Width::DWord, [0xc1]+/5, r / m32, imm8);
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
            Sar::try_parse(b"\xc1\xf9\x3f", PrefixBytes::new_rex(0x48), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Sar,
                    width: Width::QWord,
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
            Shr::try_parse(b"\xc1\xe9\x37", PrefixBytes::new_rex(0x48), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Shr,
                    width: Width::QWord,
                    op_1: Some(OpReg(rcx())),
                    op_2: Some(OpImm(Immediate::Byte(55))),
                    op_3: None
                }
            )),
            "48 c1 e9 37     shrq    $55, %rcx"
        );
    }

    #[test]
    fn instr_shl_c0() {
        assert_eq!(
            Shl::try_parse(b"\xc0\xe1\x04", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Shl,
                    width: Width::Byte,
                    op_1: Some(OpReg(cl())),
                    op_2: Some(OpImm(Immediate::Byte(4))),
                    op_3: None
                }
            )),
            "c0 e1 04        shlb    $4, %cl"
        );
    }

    #[test]
    fn instr_shl_c1() {
        assert_eq!(
            Shl::try_parse(b"\xc1\xe4\x05", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Shl,
                    width: Width::DWord,
                    op_1: Some(OpReg(r12d())),
                    op_2: Some(OpImm(Immediate::Byte(5))),
                    op_3: None
                }
            )),
            "41 c1 e4 05     shll    $5, %r12d"
        );
    }
}
