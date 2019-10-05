use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Cmove {}

impl DecodeInstruction for Cmove {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        call!(input, Cmove::parse_x0f44, prefix, address)
    }
}

impl Cmove {
    // 0f 44 /r             => CMOVE r16, r/m16
    // 0f 44 /r             => CMOVE r32, r/m32
    // REX.W + 0f 44 /r     => CMOVE r64, r/m64
    instr!(parse_x0f44, Opcode::Cmove, Width::DWord, [0x0f, 0x44], /r32, r/m32);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Cmovne {}

impl DecodeInstruction for Cmovne {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        call!(input, Cmovne::parse_x0f45, prefix, address)
    }
}

impl Cmovne {
    // 0f 45 /r             => CMOVNE r16, r/m16
    // 0f 45 /r             => CMOVNE r32, r/m32
    // REX.W + 0f 45 /r     => CMOVNE r64, r/m64
    instr!(parse_x0f45, Opcode::Cmovne, Width::DWord, [0x0f, 0x45], /r32, r/m32);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Cmovns {}

impl DecodeInstruction for Cmovns {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        call!(input, Cmovns::parse_x0f49, prefix, address)
    }
}

impl Cmovns {
    // 0f 49 /r             => CMOVNS r16, r/m16
    // 0f 41 /r             => CMOVNS r32, r/m32
    // REX.W + 0f 49 /r     => CMOVNS r64, r/m64
    instr!(parse_x0f49, Opcode::Cmovns, Width::DWord, [0x0f, 0x49], /r32, r/m32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{instr::Operand::Register as OpReg, register::ctors::*};

    #[test]
    fn instr_cmove() {
        assert_eq!(
            Cmove::try_parse(b"\x0f\x44\xd8", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Cmove,
                    width: Width::DWord,
                    op_1: Some(OpReg(ebx())),
                    op_2: Some(OpReg(eax())),
                    op_3: None
                }
            )),
            "0f 44 d8        cmovel  %eax, %ebx"
        );
    }

    #[test]
    fn instr_cmovne() {
        assert_eq!(
            Cmovne::try_parse(b"\x0f\x45\xe1", PrefixBytes::new_rex(0x44), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Cmovne,
                    width: Width::DWord,
                    op_1: Some(OpReg(r12d())),
                    op_2: Some(OpReg(ecx())),
                    op_3: None
                }
            )),
            "44 0f 45 e1     cmovnel %ecx, %r12d"
        );

        assert_eq!(
            Cmovne::try_parse(b"\x0f\x45\xf4", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Cmovne,
                    width: Width::DWord,
                    op_1: Some(OpReg(esi())),
                    op_2: Some(OpReg(r12d())),
                    op_3: None
                }
            )),
            "41 0f 45 f4     cmovnel %r12d, %esi"
        );
    }

    #[test]
    fn instr_cmovns() {
        assert_eq!(
            Cmovns::try_parse(b"\x0f\x49\xe8", PrefixBytes::new_rex(0x4c), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Cmovns,
                    width: Width::QWord,
                    op_1: Some(OpReg(r13())),
                    op_2: Some(OpReg(rax())),
                    op_3: None
                }
            )),
            "4c 0f 49 e8     cmovnsq %rax, %r13"
        );
    }
}
