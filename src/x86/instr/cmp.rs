use nom::*;

use crate::x86::{
    instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes},
    Width,
};

#[derive(Debug, PartialEq)]
pub(crate) struct Cmp {}

impl DecodeInstruction for Cmp {
    fn try_parse(input: &[u8], prefix: PrefixBytes, address: usize) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Cmp::parse_x38, prefix, address)
                | call!(Cmp::parse_x39, prefix, address)
                | call!(Cmp::parse_x3b, prefix, address)
                | call!(Cmp::parse_x3d, prefix, address)
                | call!(Cmp::parse_x80, prefix, address)
                | call!(Cmp::parse_x81, prefix, address)
                | call!(Cmp::parse_x83, prefix, address)
        )
    }
}

impl Cmp {
    // 38 /r            => CMP r/m8, r8
    // REX c 38 /r      => CMP r/m8, r8
    instr!(parse_x38, Opcode::Cmp, Width::Byte, [0x38], r/m8, /r8);

    // 39 /r            => CMP r/m16, r16
    // 39 /r            => CMP r/m32, r32
    // REX.W + 39 /r    => CMP r/m64, r64
    instr!(parse_x39, Opcode::Cmp, Width::DWord, [0x39], r/m32, /r32);

    // 3b /r            => CMP r16, r/m16
    // 3b /r            => CMP r32, r/m32
    // REX.W + 3b /r    => CMP r64, r/m64
    instr!(parse_x3b, Opcode::Cmp, Width::DWord, [0x3b], /r32, r/m32);

    // 3d iw            => CMP AX, imm16
    // 3d id            => CMP EAX, imm32
    // REX.W + 3d id    => CMP RAX, imm32
    instr!(
        parse_x3d,
        Opcode::Cmp,
        Width::DWord,
        [0x3d],
        reg: eax,
        imm32
    );

    // 80 /7 ib             => CMP r/m8, imm8
    // REX + 80 /7 ib       => CMP r/m8, imm8
    instr!(parse_x80, Opcode::Cmp, Width::Byte, [0x80]+/7, r/m8, imm8);

    // 81 /7 iw             => CMP r/m16, imm16
    // 81 /7 id             => CMP r/m32, imm32
    // REX.W + 81 /7 id     => CMP r/m64, imm32
    instr!(parse_x81, Opcode::Cmp, Width::DWord, [0x81]+/7, r/m32, imm32);

    // 83 /7 ib             => CMP r/m16, imm8
    // 83 /7 ib             => CMP r/m32, imm8
    // REX.W + 83 /7 ib     => CMP r/m64, imm8
    instr!(parse_x83, Opcode::Cmp, Width::Byte, [0x83]+/7, r/m32, imm8);
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
    fn instr_cmp_39() {
        assert_eq!(
            Cmp::try_parse(b"\x39\x48\x30", PrefixBytes::new_rex(0x49), 10),
            Ok((
                &b""[..],
                Instruction {
                    address: 10,
                    opcode: Opcode::Cmp,
                    width: Width::QWord,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(r8()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::Byte(48_i8))
                        }
                    })),
                    op_2: Some(OpReg(rcx())),
                    op_3: None
                }
            )),
            "49 39 48 30 	cmpq	%rcx, 48(%r8)"
        )
    }

    #[test]
    fn instr_cmp_3b() {
        assert_eq!(
            Cmp::try_parse(b"\x3b\x85\x68\xfb\xff\xff", PrefixBytes::new_rex(0x48), 10),
            Ok((
                &b""[..],
                Instruction {
                    address: 10,
                    opcode: Opcode::Cmp,
                    width: Width::QWord,
                    op_1: Some(OpReg(rax())),
                    op_2: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rbp()),
                            index: None,
                            scale: None,
                            displacement: Some(Displacement::DWord(-1176))
                        }
                    })),
                    op_3: None
                }
            )),
            "48 3b 85 68 fb ff ff    cmpq    -1176(%rbp), %rax"
        )
    }

    #[test]
    fn instr_cmp_80() {
        assert_eq!(
            Cmp::try_parse(b"\x80\x38\x00", PrefixBytes::new_none(), 10),
            Ok((
                &b""[..],
                Instruction {
                    address: 10,
                    opcode: Opcode::Cmp,
                    width: Width::Byte,
                    op_1: Some(OpMem(LogicalAddress {
                        segment: None,
                        offset: EffectiveAddress {
                            base: Some(rax()),
                            index: None,
                            scale: None,
                            displacement: None
                        }
                    })),
                    op_2: Some(OpImm(Immediate::Byte(0))),
                    op_3: None
                }
            )),
            "80 38 00        cmpb    $0, (%rax)"
        )
    }

    #[test]
    fn instr_cmp_81() {
        assert_eq!(
            Cmp::try_parse(b"\x81\xff\xff\x00\x00\x00", PrefixBytes::new_none(), 10),
            Ok((
                &b""[..],
                Instruction {
                    address: 10,
                    opcode: Opcode::Cmp,
                    width: Width::DWord,
                    op_1: Some(OpReg(edi())),
                    op_2: Some(OpImm(Immediate::DWord(255))),
                    op_3: None
                }
            )),
            "81 ff ff 00 00 00       cmpl    $255, %edi"
        )
    }
}
