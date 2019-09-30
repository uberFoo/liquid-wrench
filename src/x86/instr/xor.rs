use nom::*;

use crate::x86::instr::{DecodeInstruction, Instruction, Opcode, PrefixBytes};

#[derive(Debug, PartialEq)]
pub(crate) struct Xor {}

impl DecodeInstruction for Xor {
    fn try_parse(input: &[u8], prefix: PrefixBytes) -> IResult<&[u8], Instruction> {
        alt!(
            input,
            call!(Xor::parse_x31, prefix)
                | call!(Xor::parse_x33, prefix)
                | call!(Xor::parse_x34, prefix)
                | call!(Xor::parse_x35, prefix)
                | call!(Xor::parse_x80, prefix)
        )
    }
}

impl Xor {
    // 31 /r            => XOR r/m16, r16
    // 31 /r            => XOR r/m32, r32
    // REX.W + 31 /r    => XOR r/m64, r64
    instr!(parse_x31, Opcode::Xor, [0x31], r/m32, /r32);

    // 33 /r            => XOR r16, r/m16
    // 33 /r            => XOR r32, r/m32
    // REX.W + 33 /r    => XOR r32, r/m64
    instr!(parse_x33, Opcode::Xor, [0x33], /r32, r/m32);

    // 34 ib            => XOR AL, imm8
    instr!(parse_x34, Opcode::Xor, [0x34], reg: al, imm8);

    // 35 iw	        => XOR AX, imm16
    // 35 iw	        => XOR EAX, imm32
    // REX.W + 35 id    => XOR RAX, imm32
    instr!(parse_x35, Opcode::Xor, [0x35], reg: eax, imm8);

    // 80 /6 ib         => XOR r/m8, imm8
    // REX + 80 /6 ib   => XOR r/m8*, imm8
    #[rustfmt::skip]
    instr!(parse_x80, Opcode::Xor, [0x80]+/6, r/m8, imm8);
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
    };

    #[test]
    fn instr_xor_31() {
        assert_eq!(
            Xor::try_parse(b"\x31\xed", PrefixBytes::new_none()),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Xor,
                    op_1: Some(OpReg(ebp())),
                    op_2: Some(OpReg(ebp())),
                    op_3: None
                }
            )),
            "31 ed 	xorl	%ebp, %ebp"
        );

        assert_eq!(
            Xor::try_parse(b"\x31\xc0", PrefixBytes::new_rex(0x45)),
            Ok((
                &b""[..],
                Instruction {
                    opcode: Opcode::Xor,
                    op_1: Some(OpReg(r8d())),
                    op_2: Some(OpReg(r8d())),
                    op_3: None
                }
            )),
            "45 31 c0 	xorl	%r8d, %r8d"
        );
    }

    #[test]
    fn instr_xor_33() {
        assert_eq!(
            Xor::try_parse(&[0x33, 0xfb], PrefixBytes::new_none()),
            Ok((
                &[][..],
                Instruction {
                    opcode: Opcode::Xor,
                    op_1: Some(OpReg(edi())),
                    op_2: Some(OpReg(ebx())),
                    op_3: None
                }
            )),
            "33 fb 	xorl	%ebx, %edi"
        );
    }

    #[test]
    fn instr_xor_80() {
        assert_eq!(
            Xor::try_parse(&[0x80, 0xf1, 01], PrefixBytes::new_none()),
            Ok((
                &[][..],
                Instruction {
                    opcode: Opcode::Xor,
                    op_1: Some(OpReg(cl())),
                    op_2: Some(OpImm(Immediate::Byte(1_i8))),
                    op_3: None
                }
            )),
            "80 f1 01 	xorb	$1, %cl"
        );
    }

    #[test]
    fn instr_xor_34() {
        assert_eq!(
            Xor::try_parse(&[0x34, 0xfa], PrefixBytes::new_none()),
            Ok((
                &[][..],
                Instruction {
                    opcode: Opcode::Xor,
                    op_1: Some(OpReg(al())),
                    op_2: Some(OpImm(Immediate::Byte(-6_i8))),
                    op_3: None
                }
            )),
            "34 fa 	xorb	$-6, %al"
        );
    }
}
