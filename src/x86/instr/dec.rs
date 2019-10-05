use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Dec {}

impl DecodeInstruction for Dec {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Dec::parse_xff, prefix, address))
    }
}

impl Dec {
    // ff /1            => DEC r/m16
    // ff /1            => DEC r/m32
    // REX.W + ff /1    => DEC r/m64
    instr!(parse_xff, Opcode::Dec, Width::DWord, [0xff]+/1, r/m32);
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
        Width,
    };

    #[test]
    fn instr_dec_ff() {
        assert_eq!(
            Dec::try_parse(b"\xff\xc9", PrefixBytes::new_rex(0x48), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Dec,
                    width: Width::QWord,
                    op_1: Some(OpReg(rcx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "48 ff c9        decq    %rcx"
        );
    }
}
