use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, REX};

#[derive(Debug, PartialEq)]
crate struct Sete {}

impl DecodeInstruction for Sete {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        call!(input, Sete::parse_x0f94, rex)
    }
}

impl Sete {
    // 0f 94            => SETE r/m8
    // REX + 0f 94      => SETE r/m8
    instr!(parse_x0f94, Opcode::Sete, [0x0f, 0x94], r / m8);
}

#[derive(Debug, PartialEq)]
crate struct Setne {}

impl DecodeInstruction for Setne {
    fn try_parse(input: &[u8], rex: Option<REX>) -> IResult<&[u8], Instruction> {
        call!(input, Setne::parse_x0f95, rex)
    }
}

impl Setne {
    // 0f 95            => SETNE r/m8
    // REX + 0f 95      => SETNE r/m8
    instr!(parse_x0f95, Opcode::Setne, [0x0f, 0x95], r / m8);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{instr::Operand::Register as OpReg, register::ctors::*};

    #[test]
    fn instr_sete() {
        assert_eq!(
            Sete::try_parse(b"\x0f\x94\xc3", None),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Setne,
                    op_1: Some(OpReg(bl())),
                    op_2: None,
                    op_3: None
                }
            )),
            "0f 94 c3        sete    %bl"
        );
    }

    #[test]
    fn instr_setne() {
        assert_eq!(
            Setne::try_parse(b"\x0f\x95\xc0", REX::new(41)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Setne,
                    op_1: Some(OpReg(r8b())),
                    op_2: None,
                    op_3: None
                }
            )),
            "41 0f 95 c0     setne   %r8b"
        );
    }
}
