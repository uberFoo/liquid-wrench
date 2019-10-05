use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Or {}

impl DecodeInstruction for Or {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Or::parse_x09, prefix, address)
                | call!(Or::parse_x0b, prefix, address)
                | call!(Or::parse_x81, prefix, address)
                | call!(Or::parse_x83, prefix, address)
        )
    }
}

impl Or {
    // 09 /r            => OR r/m16, r16
    // 09 /r            => OR r/m32, r32
    // REX.W + 09 /r    => OR r/m64, r64
    instr!(parse_x09, Opcode::Or, Width::DWord, [0x09], r/m32, /r32);

    // 0b /r            => OR r16, r/m16
    // 0b /r            => OR r32, r/m32
    // REX.W + 0b /r    => OR r64, r/m64
    instr!(parse_x0b, Opcode::Or, Width::DWord, [0x0b], /r32, r/m32);

    // 81 /1 /iw            => OR r/m16, imm16
    // 81 /1 /id            => OR r/m32, imm32
    // REX.W + 81 /1 id     => OR r/m64, imm32
    instr!(parse_x81, Opcode::Or, Width::DWord, [0x81]+/1, r/m32, imm32);

    // 83 /1 ib         => OR r/m16, imm8
    // 83 /1 ib         => OR r/m32, imm8
    // REX.W + 83 /1 ib => OR r/m64, imm8
    instr!(parse_x83, Opcode::Or, Width::DWord, [0x83]+/1, r/m32, imm8);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::x86::{
        instr::{
            Displacement, EffectiveAddress, Immediate, LogicalAddress,
            Operand::{Immediate as OpImm, Memory as OpMem, Register as OpReg},
        },
        register::ctors::*,
    };

    #[test]
    fn instr_or_0b() {
        assert_eq!(
            Or::try_parse(b"\x0b\x05\x09\x47\x00\x00", PrefixBytes::new_none(), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Or,
                    width: Width::DWord,
                    op_1: Some(OpReg(eax())),
                    op_2: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rip()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::DWord(18185))
                        }
                    })),
                    op_3: None
                }
            )),
            "0b 05 09 47 00 00       orl     18185(%rip), %eax"
        );
    }

    #[test]
    fn instr_or_83() {
        assert_eq!(
            Or::try_parse(b"\x83\xcc\x01", PrefixBytes::new_rex(0x41), 0),
            Ok((
                &b""[..],
                Instruction {
                    address: 0,
                    opcode: Opcode::Or,
                    width: Width::DWord,
                    op_1: Some(OpReg(r12d())),
                    op_2: Some(OpImm(Immediate::Byte(1))),
                    op_3: None
                }
            )),
            "41 83 cc 01     orl     $1, %r12d"
        );
    }
}
