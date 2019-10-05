use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Sete {}

impl DecodeInstruction for Sete {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        call!(input, Sete::parse_x0f94, prefix, address)
    }
}

impl Sete {
    // 0f 94            => SETE r/m8
    // REX + 0f 94      => SETE r/m8
    instr!(parse_x0f94, Opcode::Sete, Width::Word, [0x0f, 0x94], r / m8);
}

#[derive(Debug, PartialEq)]
pub(crate) struct Setne {}

impl DecodeInstruction for Setne {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        call!(input, Setne::parse_x0f95, prefix, address)
    }
}

impl Setne {
    // 0f 95            => SETNE r/m8
    // REX + 0f 95      => SETNE r/m8
    instr!(
        parse_x0f95,
        Opcode::Setne,
        Width::Word,
        [0x0f, 0x95],
        r / m8
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{instr::Operand::Register as OpReg, register::ctors::*};

    #[test]
    fn instr_sete() {
        assert_eq!(
            Sete::try_parse(b"\x0f\x94\xc3", PrefixBytes::new_none(), 88),
            Ok((
                &b""[..],
                Instruction {
                    address: 88,
                    opcode: Opcode::Sete,
                    width: Width::Word,
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
            Setne::try_parse(b"\x0f\x95\xc0", PrefixBytes::new_rex(0x41), 88),
            Ok((
                &b""[..],
                Instruction {
                    address: 88,
                    opcode: Opcode::Setne,
                    width: Width::Word,
                    op_1: Some(OpReg(r8b())),
                    op_2: None,
                    op_3: None
                }
            )),
            "41 0f 95 c0     setne   %r8b"
        );
    }
}
