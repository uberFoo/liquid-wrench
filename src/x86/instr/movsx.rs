use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Movsx {}

impl DecodeInstruction for Movsx {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(input, call!(Movsx::parse_x63, prefix))
    }
}

impl Movsx {
    // REX.W + 63 /r        => MOV r64, r/m32
    instr!(parse_x63, Opcode::Movsx, Width::DWord, [0x63], /r64, r/m32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{
            EffectiveAddress, LogicalAddress,
            Operand::{Memory as OpMem, Register as OpReg},
            ScaleValue,
        },
        register::ctors::*,
    };

    #[test]
    fn instr_mov_63() {
        assert_eq!(
            Movsx::try_parse(b"\x63\x04\x8b", PrefixBytes::new_rex(0x48)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Movsx,
                    width: Width::QWord,
                    op_1: Some(OpReg(rax())),
                    op_2: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rbx()),
                            index: Some(rcx()),
                            scale: Some(ScaleValue::Four),
                            displacement: None
                        }
                    })),
                    op_3: None,
                }
            )),
            "48 63 04 8b     movslq  (%rbx,%rcx,4), %rax"
        );
    }
}
