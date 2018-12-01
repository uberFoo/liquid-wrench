use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, ModRM, REX},
    Instruction, Opcode,
    Operand::Register as OpReg,
    Register, Register32,
};

#[derive(Debug, PartialEq)]
crate struct Xor {}

impl DecodeInstruction for Xor {
    #[allow(clippy::cyclomatic_complexity)]
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        do_parse!(
            input,
            tag!(b"\x31")
                >> modrm: call!(ModRM::new, rex)
                >> (Instruction {
                    opcode: Opcode::Xor,
                    op_1: Some(modrm.r_m32()),
                    op_2: Some(modrm.r_32()),
                    op_3: None
                })
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instr_xor_31() {
        // 31 ed 	xorl	%ebp, %ebp
        assert_eq!(
            Xor::try_parse(b"\x31\xed", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Xor,
                    op_1: Some(OpReg(Register::DWord(Register32::EBP))),
                    op_2: Some(OpReg(Register::DWord(Register32::EBP))),
                    op_3: None
                }
            ))
        );

        assert_eq!(
            Xor::try_parse(b"\x31\xdd", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Xor,
                    op_1: Some(OpReg(Register::DWord(Register32::EBP))),
                    op_2: Some(OpReg(Register::DWord(Register32::EBX))),
                    op_3: None
                }
            ))
        );
    }
}
