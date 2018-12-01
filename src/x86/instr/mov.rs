use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, ModRM, REX},
    Instruction, Opcode,
};

#[derive(Debug, PartialEq)]
crate struct Mov {}

impl DecodeInstruction for Mov {
    #[allow(clippy::cyclomatic_complexity)]
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        do_parse!(
            input,
            tag!(b"\x89")
                >> modrm: call!(ModRM::new, rex)
                >> (Instruction {
                    opcode: Opcode::Mov,
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

    use crate::x86::{ctors::*, Operand::Register as OpReg};

    #[test]
    fn instr_mov_89() {
        assert_eq!(
            Mov::try_parse(b"\x89\xe2", REX::new(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    op_1: Some(OpReg(rdx())),
                    op_2: Some(OpReg(rsp())),
                    op_3: None
                }
            )),
            "48 89 e2                mov    rdx, rsp"
        );

        assert_eq!(
            Mov::try_parse(b"\x89\xf6", REX::new(0x49)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Mov,
                    op_1: Some(OpReg(r14())),
                    op_2: Some(OpReg(rsi())),
                    op_3: None
                }
            )),
            "49 89 f6                mov    r14, rsi"
        );
    }
}
