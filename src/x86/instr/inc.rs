use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Inc {}

impl DecodeInstruction for Inc {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Inc::parse_xff, prefix))
    }
}

impl Inc {
    // ff /0            => INC r/m16
    // ff /0            => INC r/m32
    // REX.W + ff /0    => INC r/m64
    instr!(parse_xff, Opcode::Inc, Width::DWord, [0xff]+/0, r/m32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{instr::Operand::Register as OpReg, register::ctors::*, Width};

    #[test]
    fn instr_inc_ff() {
        assert_eq!(
            Inc::try_parse(b"\xff\xc3", PrefixBytes::new_rex(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Inc,
                    width: Width::QWord,
                    op_1: Some(OpReg(rbx())),
                    op_2: None,
                    op_3: None
                }
            )),
            "48 ff c3        incq    %rbx"
        );
    }
}
