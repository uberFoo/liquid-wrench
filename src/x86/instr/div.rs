use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Div {}

impl DecodeInstruction for Div {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Div::parse_xf7, prefix))
    }
}

impl Div {
    // f7 /6            => DIV r/m16
    // f7 /6            => DIV r/m32
    // REX.W + f7 /6    => DIV r/m64
    instr!(parse_xf7, Opcode::Div, Width::DWord, [0xf7]+/6, r/m32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{EffectiveAddress, LogicalAddress, Operand::Memory as OpMem},
        register::ctors::*,
    };

    #[test]
    fn instr_div_f7() {
        assert_eq!(
            Div::try_parse(b"\xf7\x31", PrefixBytes::new_rex(48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Div,
                    width: Width::DWord,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rcx()),
                            index: None,
                            scale: None,
                            displacement: None
                        }
                    })),

                    op_2: None,
                    op_3: None
                }
            )),
            "48 f7 31        divq    (%rcx)"
        );
    }
}
