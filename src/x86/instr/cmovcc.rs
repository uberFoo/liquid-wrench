use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
crate struct Cmove {}

impl DecodeInstruction for Cmove {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        call!(input, Cmove::parse_x0f44, rex)
    }
}

impl Cmove {
    // 0f 44 /r             => CMOVNE r16, r/m16
    // 0f 44 /r             => CMOVNE r32, r/m32
    // REX.W + 0f 44 /r     => CMOVNE r64, r/m64
    instr!(parse_x0f44, Opcode::Cmove, [0x0f, 0x44], /r32, r/m32);
}

#[derive(Debug, PartialEq)]
crate struct Cmovne {}

impl DecodeInstruction for Cmovne {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        call!(input, Cmovne::parse_x0f45, rex)
    }
}

impl Cmovne {
    // 0f 45 /r             => CMOVNE r16, r/m16
    // 0f 45 /r             => CMOVNE r32, r/m32
    // REX.W + 0f 45 /r     => CMOVNE r64, r/m64
    instr!(parse_x0f45, Opcode::Cmovne, [0x0f, 0x45], /r32, r/m32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{instr::Operand::Register as OpReg, register::ctors::*};

    #[test]
    fn instr_cmove() {
        assert_eq!(
            Cmove::try_parse(b"\x0f\x44\xd8", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Cmove,
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
            Cmovne::try_parse(b"\x0f\x45\xe1", REX::new(44)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Cmovne,
                    op_1: Some(OpReg(r12d())),
                    op_2: Some(OpReg(ecx())),
                    op_3: None
                }
            )),
            "44 0f 45 e1     cmovnel %ecx, %r12d"
        );

        assert_eq!(
            Cmovne::try_parse(b"\x0f\x45\xf4", REX::new(41)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Cmovne,
                    op_1: Some(OpReg(esi())),
                    op_2: Some(OpReg(r12d())),
                    op_3: None
                }
            )),
            "41 0f 45 f4     cmovnel %r12d, %esi"
        );
    }
}
