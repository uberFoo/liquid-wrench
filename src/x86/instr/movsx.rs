use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Movsx {}

impl DecodeInstruction for Movsx {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Movsx::parse_x0fbe, prefix, address) | call!(Movsx::parse_x63, prefix, address)
        )
    }
}

impl Movsx {
    // 0f be /r             => MOVSX r16, r/m8
    // 0f be /r             => MOVSX r32, r/m8
    // REX + 0f be /r       => MOVSX r64, r/m8
    instr!(parse_x0fbe, Opcode::Movsx, Width::Byte, [0x0f, 0xbe], /r32, r/m8);

    // REX.W + 63 /r        => MOV r64, r/m32
    instr!(parse_x63, Opcode::Movsx, Width::DWord, [0x63], /r64, r/m32);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{
            Displacement, EffectiveAddress, LogicalAddress,
            Operand::{Memory as OpMem, Register as OpReg},
            ScaleValue,
        },
        register::ctors::*,
    };

    #[test]
    fn instr_mov_0fbe() {
        assert_eq!(
            Movsx::try_parse(b"\x0f\xbe\x7d\x01", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Movsx,
                    width: Width::Byte,
                    op_1: Some(OpReg(edi())),
                    op_2: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(r13()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(1))
                        }
                    })),
                    op_3: None,
                }
            )),
            "41 0f be 7d 01  movsbl  1(%r13), %edi"
        );
    }

    #[test]
    fn instr_mov_63() {
        assert_eq!(
            Movsx::try_parse(b"\x63\x04\x8b", PrefixBytes::new_rex(0x48), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
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
