use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
crate struct Mov {}

impl DecodeInstruction for Mov {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Mov::parse_x89, rex))
    }
}

impl Mov {
    // 89 /r            => MOV r/m16,r16
    // 89 /r            => MOV r/m32,r32
    // REX.W + 89 /r    => MOV r/m64,r64
    instr!(parse_x89, Opcode::Mov, [0x89], r/m32, /r32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{instr::Operand::Register as OpReg, register::ctors::*};

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
